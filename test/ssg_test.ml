open Subux_lib
open Domain

let fail msg =
  prerr_endline ("FAIL: " ^ msg);
  exit 1

let assert_true (cond : bool) (msg : string) : unit =
  if not cond then fail msg

let assert_eq_str (actual : string) (expected : string) (msg : string) : unit =
  if actual <> expected then
    fail
      (Printf.sprintf "%s\n  Expected: %S\n  Actual:   %S" msg expected actual)

let assert_contains (haystack : string) (needle : string) (msg : string) : unit
    =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else loop (i + 1)
  in
  if nlen > 0 && not (loop 0) then
    fail
      (Printf.sprintf "%s\n  Missing substring: %S\n  In output:\n%s" msg needle
         haystack)

let assert_not_contains (haystack : string) (needle : string) (msg : string) :
    unit =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else loop (i + 1)
  in
  if nlen > 0 && loop 0 then
    fail
      (Printf.sprintf "%s\n  Forbidden substring found: %S\n  In output:\n%s"
         msg needle haystack)

(* ==========================================================================
   1. Frontmatter Security & Parsing Tests
   ========================================================================== *)
let test_frontmatter () =
  (* 1.1 preserves YAML metadata, timestamp dates and Markdown body *)
  begin match
    Frontmatter.parse ~file:"test.md"
      "---\ntitle: 글 제목\ndate: 2026-10-09\nseriesOrder: 2\n---\n\n# Body\n"
  with
  | Error e -> fail ("1.1 unexpected error: " ^ error_to_string e)
  | Ok doc ->
      assert_eq_str
        (Option.value ~default:"" doc.meta.title)
        "글 제목" "1.1 title";
      assert_true (doc.meta.series_order = Some 2) "1.1 seriesOrder";
      assert_true
        (doc.meta.date = Some { year = 2026; month = 10; day = 9 })
        "1.1 date";
      assert_eq_str doc.body "\n# Body\n" "1.1 body"
  end;

  (* 1.2 preserves BOM, CRLF, empty metadata and ordinary Markdown handling *)
  begin match
    Frontmatter.parse ~file:"bom.md"
      "\xEF\xBB\xBF---\r\ntitle: Hello\r\n---\r\nBody"
  with
  | Error e -> fail ("1.2 BOM error: " ^ error_to_string e)
  | Ok doc ->
      assert_eq_str
        (Option.value ~default:"" doc.meta.title)
        "Hello" "1.2 BOM title";
      assert_eq_str doc.body "Body" "1.2 BOM body"
  end;

  begin match Frontmatter.parse ~file:"empty.md" "---\n---\nBody" with
  | Error e -> fail ("1.2 empty fm error: " ^ error_to_string e)
  | Ok doc ->
      assert_true (doc.meta.title = None) "1.2 empty fm title";
      assert_eq_str doc.body "Body" "1.2 empty fm body"
  end;

  begin match Frontmatter.parse ~file:"no-fm.md" "\xEF\xBB\xBF# Body" with
  | Error e -> fail ("1.2 no fm error: " ^ error_to_string e)
  | Ok doc -> assert_eq_str doc.body "# Body" "1.2 no fm body"
  end;

  begin match
    Frontmatter.parse ~file:"yaml-fence.md" "---yaml\ntitle: Hello\n---\n"
  with
  | Error e -> fail ("1.2 ---yaml error: " ^ error_to_string e)
  | Ok doc ->
      assert_eq_str
        (Option.value ~default:"" doc.meta.title)
        "Hello" "1.2 ---yaml title"
  end;

  (* 1.3 retains repair for ambiguous colons without changing quoted values *)
  begin match
    Frontmatter.parse ~file:"colon.md"
      "---\ntitle: Architecture: System overview # note\ndescription: \"Already: quoted\"\nurl: https://example.com\n---\n"
  with
  | Error e -> fail ("1.3 colon repair error: " ^ error_to_string e)
  | Ok doc ->
      assert_eq_str
        (Option.value ~default:"" doc.meta.title)
        "Architecture: System overview" "1.3 colon title";
      assert_eq_str
        (Option.value ~default:"" doc.meta.description)
        "Already: quoted" "1.3 colon description";
      assert_eq_str
        (Option.value ~default:"" (List.assoc_opt "url" doc.meta.extra_fields))
        "https://example.com" "1.3 colon url"
  end;

  (* 1.4 rejects executable frontmatter languages and JavaScript tags *)
  begin match
    Frontmatter.parse ~file:"js.md"
      "---js\n({title: (process.title = \"unsafe\")})\n---\n"
  with
  | Ok _ -> fail "1.4 ---js should be rejected"
  | Error _ -> ()
  end;

  begin match
    Frontmatter.parse ~file:"tag.md"
      "---\ntitle: !!js/function function() {}\n---\n"
  with
  | Ok _ -> fail "1.4 !!js/function should be rejected"
  | Error _ -> ()
  end;

  (* 1.5 rejects aliases, cyclic metadata, invalid YAML and non-mapping metadata *)
  List.iter
    (fun bad_body ->
      match
        Frontmatter.parse ~file:"bad.md" ("---\n" ^ bad_body ^ "\n---\n")
      with
      | Ok _ -> fail ("1.5 expected rejection for: " ^ bad_body)
      | Error _ -> ())
    [
      "base: &base [one, two]\ncopy: *base";
      "cycle: &cycle [*cycle]";
      "title: [unclosed";
      "- item";
      "plain scalar";
    ];

  begin match Frontmatter.parse ~file:"unclosed.md" "---\ntitle: Unclosed" with
  | Ok _ -> fail "1.5 unclosed frontmatter should fail"
  | Error _ -> ()
  end;

  print_endline "PASS: Frontmatter security and parsing tests"

(* ==========================================================================
   2. Markdown Rendering Tests
   ========================================================================== *)
let render_html (md : string) : string = (Markdown.render md).html

let test_markdown_rendering () =
  (* 2.1 wiki links retain aliases, nested paths and fragment targets *)
  let html_wiki =
    render_html
      "[[System Architecture|별칭]] [[2026/Guide#section]] [[already%20encoded]]"
  in
  assert_contains html_wiki
    "class=\"internal new\" href=\"/system-architecture\">별칭</a>" "2.1 wiki alias";
  assert_contains html_wiki "href=\"/2026/guide#section\"" "2.1 wiki nested fragment";
  assert_contains html_wiki "href=\"/already%20encoded\""
    "2.1 wiki already encoded";

  (* 2.2 wiki links cannot become external hosts through slash or control prefixes *)
  List.iter
    (fun target ->
      let h = render_html ("[[" ^ target ^ "]]") in
      assert_contains h "href=\"/evil.com" ("2.2 wiki safe host: " ^ target);
      assert_not_contains h "href=\"//evil.com"
        ("2.2 wiki no protocol-relative host: " ^ target))
    [ "//evil.com"; "\\evil.com"; "\t//evil.com"; "//evil.com/path" ];
  assert_contains
    (render_html "[External](https://example.com)")
    "href=\"https://example.com\"" "2.2 external normal link";

  (* 2.3 callouts retain titles, first-paragraph formatting, links and following paragraphs *)
  let html_callout =
    render_html
      "> [!warning]+ **주의**\n> **본문**과 [링크](/post)입니다.\n>\n> 두 번째 문단"
  in
  assert_contains html_callout
    "class=\"callout-warning\" data-callout=\"warning\" data-expandable=\"true\" data-expanded=\"true\""
    "2.3 callout attributes";
  assert_contains html_callout
    "class=\"callout-title-text\"><strong>주의</strong>" "2.3 callout title";
  assert_contains html_callout "class=\"callout-content\">"
    "2.3 callout content wrapper";
  assert_contains html_callout
    "<strong>본문</strong>과 <a href=\"/post\">링크</a>입니다."
    "2.3 callout first paragraph body";
  assert_contains html_callout "<p>두 번째 문단</p>"
    "2.3 callout second paragraph";

  (* 2.4 callouts support defaults and nested content without admitting raw HTML *)
  let html_nested =
    render_html
      "> [!unknown]- 제목 <img src=x onerror=alert(1)>\n> Body\n>\n> > [!tip] 내부\n> > [unsafe](javascript:alert%281%29)"
  in
  assert_contains html_nested
    "class=\"callout-note\" data-callout=\"note\" data-expandable=\"true\" data-expanded=\"false\""
    "2.4 unknown callout defaults to note";
  assert_contains html_nested "class=\"callout-tip\"" "2.4 nested tip callout";
  assert_contains html_nested "<p>Body</p>" "2.4 outer body";
  assert_contains html_nested "<a>unsafe</a>" "2.4 disarmed unsafe link";
  assert_not_contains html_nested "<img" "2.4 no raw HTML img";
  assert_not_contains html_nested "javascript:" "2.4 no javascript scheme";

  (* 2.5 GFM tables, task lists and strikethrough render as semantic HTML *)
  let html_gfm =
    render_html
      "| Name | Count |\n| :--- | ---: |\n| Pod | 2 |\n\n- [x] Done\n\n~~Removed~~"
  in
  assert_contains html_gfm "<table>" "2.5 table";
  assert_contains html_gfm "<th align=\"left\">Name</th>" "2.5 th align left";
  assert_contains html_gfm "<td align=\"right\">2</td>" "2.5 td align right";
  assert_contains html_gfm "<input type=\"checkbox\" checked disabled>"
    "2.5 task list checkbox";
  assert_contains html_gfm "<del>Removed</del>" "2.5 strikethrough";

  (* 2.6 syntax highlighting preserves aliases and leaves text and Mermaid fences untouched *)
  let html_code =
    render_html
      "```sh\necho \"hello\"\n```\n\n```yml\nname: test\n```\n\n```json\n{\"ready\":true}\n```\n\n```text\nplain <text>\n```\n\n```mermaid\ngraph LR; A --> B\n```"
  in
  assert_contains html_code "class=\"hljs language-sh\"" "2.6 sh class";
  assert_contains html_code "class=\"hljs-string\">\"hello\"</span>"
    "2.6 sh string";
  assert_contains html_code "class=\"hljs language-yml\"" "2.6 yml class";
  assert_contains html_code "class=\"hljs-attr\">name:</span>" "2.6 yml attr";
  assert_contains html_code "class=\"hljs language-json\"" "2.6 json class";
  assert_contains html_code "<code class=\"language-text\">plain &#x3C;text>"
    "2.6 text fence untouched";
  assert_contains html_code "<code class=\"language-mermaid\">graph LR; A --> B"
    "2.6 mermaid fence untouched";

  (* 2.7 footnotes retain target links and plain-text hover content inside callouts *)
  let html_fn =
    render_html
      "> [!note] 각주\n> 문장[^1]\n\n[^1]: **설명**과 [링크](/post)."
  in
  assert_contains html_fn "data-footnote-content=\"설명과 링크.\""
    "2.7 footnote tooltip";
  assert_contains html_fn "href=\"#user-content-fn-1\"" "2.7 footnote href";
  assert_contains html_fn "id=\"user-content-fn-1\"" "2.7 footnote target id";
  assert_contains html_fn "href=\"#user-content-fnref-1\""
    "2.7 footnote backref";

  print_endline
    "PASS: Markdown rendering tests"

(* ==========================================================================
   3. Safe URLs & Video Directive Tests
   ========================================================================== *)
let test_safe_urls () =
  (* 3.1 removes executable and data URLs including browser-normalized obfuscations *)
  List.iter
    (fun url ->
      let html = render_html ("[link](" ^ url ^ ")\n\n![image](" ^ url ^ ")") in
      assert_eq_str html "<p><a>link</a></p>\n<p><img alt=\"image\"></p>"
        ("3.1 unsafe url disarmed: " ^ url))
    [
      "javascript:alert%281%29";
      "JAVASCRIPT:alert%281%29";
      "data:text/html,unsafe";
      "vbscript:msgbox%281%29";
      "file:///etc/passwd";
    ];

  (* 3.2 Markdown-encoded control characters remain inert relative paths *)
  let html_ctrl = render_html "[link](java&#x09;script:alert%281%29)" in
  assert_eq_str html_ctrl "<p><a href=\"java%09script:alert%281%29\">link</a></p>"
    "3.2 encoded tab in link destination";

  (* 3.3 preserves relative, fragment, network, HTTP and contact links *)
  List.iter
    (fun url ->
      let html = render_html ("[link](" ^ url ^ ")") in
      assert_eq_str html
        ("<p><a href=\"" ^ url ^ "\">link</a></p>")
        ("3.3 safe url preserved: " ^ url))
    [
      "/2026/getting-started";
      "../post";
      "#user-content-fn-1";
      "//example.com/page";
      "https://example.com/page";
      "http://example.com/page";
      "mailto:reader@example.com";
      "tel:+821012345678";
    ];

  (* 3.4 preserves existing MP4 videos and rejects unsafe video sources *)
  let safe_video =
    render_html
      "::video{src=\"https://cdn.example.com/demo.mp4\" title=\"시연 영상\"}"
  in
  assert_contains safe_video "src=\"https://cdn.example.com/demo.mp4\""
    "3.4 video src";
  List.iter
    (fun attr -> assert_contains safe_video attr ("3.4 video attr " ^ attr))
    [ "controls"; "muted"; "playsinline"; "preload=\"metadata\""; "class=\"article-video\""; "aria-label=\"시연 영상\"" ];

  List.iter
    (fun bad_url ->
      let unsafe_video =
        render_html ("::video{src=\"" ^ bad_url ^ "\"}")
      in
      assert_not_contains unsafe_video " src="
        ("3.4 unsafe video src rejected: " ^ bad_url))
    [
      "javascript:alert(1)";
      "java&#x09;script:alert(1)";
      "java\tscript:alert(1)";
      "java&#x0a;script:alert(1)";
      "  JAVASCRIPT:alert(1)";
      "data:text/html,unsafe";
      "mailto:x@y.z";
    ];

  (* 3.5 preserves footnote targets, tooltips and Markdown formatting *)
  let html_fn =
    render_html "**Text**[^1]\n\n[^1]: Note with [link](/post)."
  in
  assert_contains html_fn "<strong>Text</strong>" "3.5 strong text";
  assert_contains html_fn "data-footnote-content=\"Note with link."
    "3.5 footnote tooltip";
  assert_contains html_fn "href=\"#user-content-fn-1\"" "3.5 fn href";
  assert_contains html_fn "id=\"user-content-fn-1\"" "3.5 fn id";
  assert_contains html_fn "href=\"/post\"" "3.5 fn inner link";

  print_endline "PASS: Safe URLs & Video directive tests"

(* ==========================================================================
   4. W3C DTCG Design Token & End-to-End Site Builder Tests
   ========================================================================== *)
let test_color_tokens_and_site_builder () =
  let tokens_json =
    In_channel.with_open_bin "../design/tokens.json" In_channel.input_all
  in
  begin match
    Token_compiler.compile_full_stylesheet ~area_tone:Primary_tone tokens_json
  with
  | Error e -> fail ("4.1 token compiler error: " ^ error_to_string e)
  | Ok css ->
      assert_contains css "--sx-color-primary: #c2e7ff;" "4.1 primary token";
      assert_contains css "--sx-color-on-primary: #1f1f1f;"
        "4.1 on-primary token";
      assert_contains css "--sx-color-accent: #ffdac2;" "4.1 accent token";
      assert_contains css "--sx-color-on-accent: #1f1f1f;" "4.1 on-accent token";
      assert_contains css "--sx-color-callout-note: #3b82f6;"
        "4.1 callout-note token";
      assert_contains css "--sx-color-code-string: #0a7f5a;"
        "4.1 code-string token";
      assert_contains css "@media (prefers-color-scheme: dark)"
        "4.1 dark media query";
      assert_contains css ":root[data-theme=\"dark\"]"
        "4.1 explicit dark theme selector";
      assert_not_contains css "--sx-palette-"
        "4.1 palette reference tokens must not be exported"
  end;

  (* 4.2 Test Blog_mode site compilation *)
  let blog_sources : Site_builder.source_file list =
    [
      {
        collection = "articles";
        rel_path = "articles/2026/FUNCTIONAL-ARCHITECTURE.md";
        raw_markdown =
          "---\ntitle: 순수 함수형 아키텍처\ndate: 2026-02-15\n---\n\n본문 [[2026/SYSTEM-DESIGN-PART-1|시스템 설계 1편]]\n";
      };
      {
        collection = "tech";
        rel_path = "tech/2026/SYSTEM-DESIGN-PART-1.md";
        raw_markdown =
          "---\ntitle: 시스템 설계 (1)\ndate: 2026-03-01\nseries: 시스템 설계 시리즈\nseriesOrder: 1\n---\n\n```mermaid\ngraph LR; A --> B\n```\n";
      };
      {
        collection = "tech";
        rel_path = "tech/2026/SYSTEM-DESIGN-PART-2.md";
        raw_markdown =
          "---\ntitle: 시스템 설계 (2)\ndate: 2026-03-10\nseries: 시스템 설계 시리즈\nseriesOrder: 2\n---\n\n2편 본문\n";
      };
      {
        collection = "translations";
        rel_path = "translations/type-driven-design.md";
        raw_markdown =
          "---\ntitle: Type-Driven Design Principles\ndate: 2026-02-10\n---\n\n번역 본문\n";
      };
    ]
  in
  begin match
    Site_builder.compile_site ~config:default_blog_config ~tokens_json
      blog_sources
  with
  | Error e -> fail ("4.2 blog compile error: " ^ error_to_string e)
  | Ok plan ->
      let find_art path =
        List.find_opt
          (fun (a : Site_builder.output_artifact) -> a.out_path = path)
          plan.artifacts
      in
      let index_art = Option.get (find_art "index.html") in
      assert_contains index_art.content "생각" "4.2 blog tab 생각";
      assert_contains index_art.content "기술" "4.2 blog tab 기술";
      assert_contains index_art.content "한국어 번역" "4.2 blog tab 한국어 번역";
      assert_contains index_art.content "번역본에 대한 모든 권리는 원문 저자에게 있습니다."
        "4.2 translations disclaimer";
      assert_contains index_art.content "2026년" "4.2 year header";
      assert_contains index_art.content "2월 15일" "4.2 short korean date";

      let part1_art =
        Option.get
          (find_art "2026/SYSTEM-DESIGN-PART-1/index.html")
      in
      assert_contains part1_art.content "시리즈 · 시스템 설계 시리즈"
        "4.2 series nav label";
      assert_contains part1_art.content "class=\"series-nav-current\""
        "4.2 series current item";
      assert_contains part1_art.content "class=\"series-nav-link\""
        "4.2 series link item";
      assert_contains part1_art.content "mermaid.esm.min.mjs"
        "4.2 mermaid lazy script injected when mermaid block present";

      let not_found_art = Option.get (find_art "404.html") in
      assert_contains not_found_art.content "이곳엔 아무것도 없습니다."
        "4.2 404 page title"
  end;

  (* 4.3 Test Docs_mode site compilation *)
  let docs_sources : Site_builder.source_file list =
    [
      {
        collection = "";
        rel_path = "index.md";
        raw_markdown =
          "---\nlayout: home\nhero:\n  name: \"Subux Docs\"\n  text: \"기술 문서 가이드\"\n  tagline: \"체계적인 기술 문서와 레퍼런스를 작성합니다\"\n  actions:\n    - theme: brand\n      text: 시작하기\n      link: /contents/getting-started\nfeatures:\n  - title: 순수 함수형 파이프라인\n    details: 예측 가능한 정적 빌드와 타입 안전성을 제공합니다.\n  - title: 간결한 정리\n    details: 핵심만 빠르게 파악할 수 있도록 정리합니다.\n---\n";
      };
      {
        collection = "";
        rel_path = "contents/getting-started.md";
        raw_markdown =
          "---\ntitle: 시작하기 가이드\norder: 1\n---\n\n# 시작하기 가이드\n\n## 핵심 원칙\n\n내용\n";
      };
      {
        collection = "";
        rel_path = "contents/configuration.md";
        raw_markdown =
          "---\ntitle: 설정 레퍼런스\norder: 2\n---\n\n# 설정 레퍼런스\n\n## 개요\n\n내용\n";
      };
    ]
  in
  begin match
    Site_builder.compile_site ~config:default_docs_config ~tokens_json
      docs_sources
  with
  | Error e -> fail ("4.3 docs compile error: " ^ error_to_string e)
  | Ok plan ->
      let find_art path =
        List.find_opt
          (fun (a : Site_builder.output_artifact) -> a.out_path = path)
          plan.artifacts
      in
      let home_art = Option.get (find_art "index.html") in
      assert_contains home_art.content "class=\"jt-hero\"" "4.3 hero section";
      assert_contains home_art.content "기술 문서 가이드" "4.3 hero text";
      assert_contains home_art.content "class=\"jt-btn jt-btn-brand\""
        "4.3 hero brand action";
      assert_contains home_art.content "class=\"jt-feature-card\""
        "4.3 feature card";

      let doc1_art =
        Option.get (find_art "contents/getting-started/index.html")
      in
      assert_contains doc1_art.content "class=\"jt-sidebar-link active\""
        "4.3 active sidebar item";
      assert_contains doc1_art.content "On this page" "4.3 right outline TOC";
      assert_contains doc1_art.content "href=\"#핵심-원칙\""
        "4.3 TOC heading anchor";
      assert_contains doc1_art.content "Next page" "4.3 auto next page pager"
  end;

  print_endline
    "PASS: Design tokens & Site_builder (Blog_mode + Docs_mode) tests"

let () =
  test_frontmatter ();
  test_markdown_rendering ();
  test_safe_urls ();
  test_color_tokens_and_site_builder ();
  print_endline "\nALL TESTS PASSED!"
