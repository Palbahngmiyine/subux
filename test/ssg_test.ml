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

  List.iter (fun title ->
    match Frontmatter.parse ("---\ntitle: " ^ title ^ " # comment\n---\nBody") with
    | Error e -> fail ("plain scalar: " ^ error_to_string e)
    | Ok doc -> assert_true (doc.meta.title = Some title) "plain title and comment")
    [ "Rob Pike's 5 Rules of Programming"; "A [plain] title" ];
  begin match Frontmatter.parse
      "---\ntags: [one, \"two # literal\", 'three # literal'] # comment\n---\n" with
  | Error e -> fail ("quoted inline list: " ^ error_to_string e)
  | Ok doc -> assert_true (doc.meta.tags = ["one"; "two # literal"; "three # literal"])
      "quoted list values retain hash after whitespace"
  end;
  List.iter (fun value ->
    match Frontmatter.parse ("---\ntitle: " ^ value ^ "\n---\n") with
    | Ok _ -> fail "malformed or unsafe title accepted"
    | Error _ -> ()) [ "'unclosed"; "\"unclosed"; "[one, two"; "Name &anchor" ];
  List.iter (fun nested ->
    match Frontmatter.parse ("---\n" ^ nested ^ "\n---\n") with
    | Ok _ -> fail ("malformed nested YAML accepted: " ^ nested)
    | Error _ -> ())
    [
      "hero:\n  name: \"Landing";
      "hero:\n  tagline: 'Unclosed";
      "features:\n  - title: \"Unfinished\n    details: ok";
      "tags:\n  - \"unfinished";
      "tags:\n  - [unclosed";
    ];
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
  let table_pipes = render_html
      "| Expression |\n| --- |\n| **`a \\|\\| <b>`** |\n\nOutside: `a \\| b`" in
  assert_contains table_pipes "<strong><code>a || &lt;b&gt;</code></strong>"
    "2.5 table code unescapes delimiter pipes and preserves escaping";
  assert_contains table_pipes "<code>a \\| b</code>"
    "2.5 code outside tables keeps literal backslashes";

  (* 2.6 syntax highlighting preserves aliases and leaves text and Mermaid fences untouched *)
  let padded_table = render_html
      "| Code |\n| --- |\n| `  foo  ` |\n| `  a \\| b  ` |\n| `   ` |\n| `` `foo` `` |\n" in
  assert_contains padded_table "<code> foo </code>"
    "table code normalizes surrounding spaces only once";
  assert_contains padded_table "<code> a | b </code>"
    "table pipe unescaping preserves significant surrounding spaces";
  assert_contains padded_table "<code>   </code>" "all-space table code stays intact";
  assert_contains padded_table "<code>`foo`</code>" "table code retains literal backticks";

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
  assert_contains html_code "class=\"sx-code-block\"" "2.6 themeable code wrapper";
  assert_contains html_code "class=\"sx-code-copy\" aria-label=\"Copy code\""
    "2.6 accessible copy control";
  let hostile_fence = render_html "```x\"<img\n<script>alert(1)</script>\n```" in
  assert_not_contains hostile_fence "<img" "2.6 language label escaped";
  assert_not_contains hostile_fence "<script>" "2.6 code text escaped";

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
  assert_contains html_fn "class=\"sr-only\">각주</h2>"
    "2.7 accessible footnote heading";
  assert_contains html_fn "aria-describedby=\"footnote-label\""
    "2.7 reference described by footnote heading";
  assert_contains html_fn "↩</a></p>" "2.7 backlink stays in the note paragraph";

  let titled = Markdown.render ~heading_base:2 ~page_title:"Page title"
      "\n# **Page title**\n\n## Section\n\nBody" in
  assert_not_contains titled.html "Page title" "template owns a matching leading title";
  assert_true (titled.page_title_id = Some "page-title") "removed H1 keeps its generated anchor";
  assert_contains titled.html "<h2 id=\"section\">" "body headings retain their hierarchy";
  assert_contains titled.html "Body" "removing a duplicate title preserves body text";
  assert_not_contains (Markdown.render ~page_title:"Page title" "Page title\n==========\n").html
    "<h1" "Setext duplicate titles are removed";
  assert_contains (Markdown.render ~heading_base:2 ~page_title:"Other" "# Page title").html
    "Page title" "a different body heading is preserved";
  assert_contains (Markdown.render ~page_title:"Page title" "Intro\n\n# Page title").html
    "<h1" "later matching headings are not removed";
  assert_true ((Markdown.render ~page_title:"Page title" "Page title\n==========\n").page_title_id
      = Some "page-title") "Setext title keeps its generated anchor";
  assert_true ((Markdown.render ~page_title:"main-content" "# main-content").page_title_id
      = Some "main-content-1") "title anchors avoid reserved template IDs";
  List.iter (fun control_id ->
    let headings = Markdown.render ("## " ^ control_id ^ "\n\n## " ^ control_id) in
    assert_contains headings.html ("id=\"" ^ control_id ^ "-1\"")
      "body headings avoid navigation and outline control IDs";
    assert_contains headings.html ("id=\"" ^ control_id ^ "-2\"")
      "repeated control-like headings keep unique anchors";
    assert_not_contains headings.html ("id=\"" ^ control_id ^ "\"")
      "body headings never claim a template control ID";
    let titled = Markdown.render ~page_title:control_id
        ("# " ^ control_id ^ "\n\n## " ^ control_id) in
    assert_true (titled.page_title_id = Some (control_id ^ "-1"))
      "removed titles also avoid control IDs";
    assert_contains titled.html ("id=\"" ^ control_id ^ "-2\"")
      "body headings do not reuse the removed title anchor")
    ["jt-main-nav"; "jt-menu-toggle"; "jt-sidebar"; "jt-sidebar-toggle";
     "jt-sidebar-backdrop"; "jt-outline"; "jt-outline-toggle"];
  assert_true ((Markdown.render ~page_title:"Other" "# Page title").page_title_id = None)
    "nonmatching titles do not allocate a template anchor";
  assert_true ((Markdown.render ~page_title:"Page title" "Intro\n\n# Page title").page_title_id = None)
    "later headings do not allocate a template anchor";

  let repeated_fn =
    render_html "First[^note] then[^note] and last[^note].\n\n[^note]: [Unsafe](javascript:alert(1))."
  in
  List.iter
    (fun suffix ->
      assert_contains repeated_fn ("id=\"user-content-fnref-note" ^ suffix ^ "\"")
        "2.7 repeated reference has a unique target";
      assert_contains repeated_fn ("href=\"#user-content-fnref-note" ^ suffix ^ "\"")
        "2.7 every reference has a backlink")
    [ ""; "-2"; "-3" ];
  assert_contains repeated_fn "aria-label=\"Back to reference 1-2\">↩<sup>2</sup></a>"
    "2.7 repeated backlink identifies its reference";
  assert_contains repeated_fn "↩<sup>3</sup></a></p>"
    "2.7 all backlinks stay in the final paragraph";
  assert_not_contains repeated_fn "#user-content-fnref-note-4"
    "2.7 no backlink to a nonexistent reference";
  assert_not_contains repeated_fn "javascript:" "2.7 repeated note keeps URL sanitization";

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
      assert_eq_str html "<p><a>link</a></p>\n<p><img loading=\"lazy\" decoding=\"async\" alt=\"image\"></p>"
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
      List.iter (fun selector -> assert_contains doc1_art.content selector
          "default critical CSS retains built-in menu states")
        [".jt-nav-links[data-open="; ".jt-sidebar[data-ready="; ".jt-outline[data-open="];
      assert_contains doc1_art.content "href=\"#핵심-원칙\""
        "4.3 TOC heading anchor";
      assert_contains doc1_art.content "Next page" "4.3 auto next page pager"
  end;

  (* A site override must be emitted as a separate stylesheet after the theme,
     for every mode; CSS content must never be interpolated into HTML. *)
  List.iter (fun mode ->
    let css = "/* </style><script> */ .jt-hero { max-width: 60rem; }" in
    let config = { default_docs_config with mode; custom_css = Some css; custom_js = Some "/* </script> */" } in
    match Site_builder.compile_site ~config ~tokens_json [] with
    | Error e -> fail ("custom stylesheet: " ^ error_to_string e)
    | Ok plan ->
        let custom = List.find_opt (fun (a : Site_builder.output_artifact) ->
          a.out_path = "assets/custom.css") plan.artifacts in
        assert_true (Option.map (fun (a : Site_builder.output_artifact) -> a.content) custom = Some css)
          "custom CSS emitted unchanged";
        assert_true (List.exists (fun (a : Site_builder.output_artifact) ->
          a.out_path = "assets/custom.js" && a.content = "/* </script> */") plan.artifacts)
          "custom module emitted unchanged";
        List.iter (fun (a : Site_builder.output_artifact) ->
          if Filename.check_suffix a.out_path ".html" then begin
            assert_contains a.content "/assets/custom.css" "custom CSS linked";
            assert_contains a.content "src=\"/assets/custom.js\"" "custom module linked";
            assert_not_contains a.content "/* </script> */" "custom module outside HTML";
            assert_not_contains a.content "<script> */" "custom CSS outside HTML";
            assert_contains a.content ".hljs-keyword" "custom scripts retain dynamic syntax styles";
            assert_contains a.content "--sx-color-callout-todo:" "custom CSS retains public tokens";
            assert_contains a.content ".jt-nav-links[data-open=" "custom scripts retain menu state styles"
          end) plan.artifacts)
    [ Blog_mode; Docs_mode; Unified_mode ];
  print_endline
    "PASS: Design tokens & Site_builder (Blog_mode + Docs_mode) tests"

let test_adapters () =
  let artifact out_path content : Site_builder.output_artifact =
    { out_path; content }
  in
  let public = [ artifact "index.html" "home"; artifact "404.html" "missing";
                 artifact "contents/한글/index.html" "guide";
                 artifact "assets/theme.css" "css" ] in
  let adapt ?(name = "my-subux") ?(date = "2026-10-29") artifacts =
    Adapter.adapt Adapter.Cloudflare ~name ~compatibility_date:date artifacts
  in
  let expect_error result message = match result with
    | Error (Config_error _) -> ()
    | Error e -> fail (message ^ ": " ^ error_to_string e)
    | Ok _ -> fail message
  in
  assert_true (Adapter.of_string "cloudflare" = Ok Adapter.Cloudflare)
    "5.1 Cloudflare adapter selection";
  assert_true (Adapter.of_string "static" = Ok Adapter.Static)
    "5.1 static adapter selection";
  expect_error (Adapter.of_string "cloudflrae") "5.1 reject unsupported adapter";
  assert_true
    (Adapter.adapt Adapter.Static ~name:"" ~compatibility_date:"" public = Ok public)
    "5.2 default static output unchanged";
  begin match adapt public with
  | Error e -> fail (error_to_string e)
  | Ok artifacts ->
      let prefix = Adapter.cloudflare_output_dir ^ "/" in
      let find path =
        match List.find_opt (fun (a : Site_builder.output_artifact) ->
          a.out_path = prefix ^ path) artifacts with
        | Some a -> a.content
        | None -> fail ("5.3 missing artifact: " ^ path)
      in
      let open Json_parser in
      assert_true (parse (find "config.json") =
        Ok (Object [ "buildContext", Object [ "isPreview", Bool false ] ]))
        "5.3 Cloudflare Build Output root schema";
      assert_true (parse (find "workers/default/worker.config.json") =
        Ok (Object [
          "name", String "my-subux";
          "compatibilityDate", String "2026-10-29";
          "workersDev", Bool true;
          "assets", Object [
            "htmlHandling", String "drop-trailing-slash";
            "notFoundHandling", String "404-page";
          ];
        ])) "5.3 assets-only Worker config preserves URLs and 404 status";
      assert_true (List.length artifacts = List.length public + 2)
        "5.3 only supplied assets and deployment configs are packaged";
      List.iter (fun (a : Site_builder.output_artifact) ->
        assert_eq_str (find ("workers/default/assets/" ^ a.out_path)) a.content
          "5.3 preserve public artifact bytes") public;
      assert_true (adapt public = Ok artifacts) "5.3 deterministic packaging"
  end;
  List.iter (fun name ->
    expect_error (adapt ~name public) "5.4 reject invalid Worker name")
    [ ""; "123"; "Uppercase"; "-site"; "site-"; "../site";
      "site\"\nworkersDev=false"; String.make 64 'a' ];
  List.iter (fun date ->
    expect_error (adapt ~date public) "5.5 reject invalid date")
    [ ""; "2026-2-01"; "2026-02-29"; "2026-04-31";
      "2026-13-01"; "2026-10-29T00:00:00"; "2026-10-29\n" ];
  assert_true (Result.is_ok (adapt ~date:"2028-02-29" public))
    "5.5 leap date accepted";
  List.iter (fun path ->
    expect_error (adapt (artifact path "private" :: public))
      "5.6 reject path traversal and unsafe paths")
    [ ""; "/private"; "../private"; "a/../private"; "./private";
      "a//b"; "a/"; "a\\b"; "a\000b"; "a\nb" ];
  expect_error (adapt (artifact "index.html" "duplicate" :: public))
    "5.7 reject duplicate artifacts";
  expect_error (adapt (artifact "assets" "file" :: public))
    "5.7 reject file/directory conflicts";
  expect_error (adapt []) "5.8 require home and 404 artifacts";
  print_endline "PASS: Static and Cloudflare adapter tests"
(* Exercise CLI I/O boundaries with real files and a bounded child process.
   In particular a FIFO must be rejected before opening it for reading. *)
let test_cli_assets () =
  let cli = Unix.realpath Sys.argv.(1) in
  let input = Filename.temp_file "subux-assets-" "" in
  Sys.remove input;
  Unix.mkdir input 0o700;
  let outside = Filename.temp_file "subux-outside-" ".css" in
  let path name = Filename.concat input name in
  let write name content =
    Out_channel.with_open_bin (path name) (fun ch -> Out_channel.output_string ch content)
  in
  let cleanup () =
    List.iter (fun name -> try Sys.remove (path name) with Sys_error _ -> ())
      [ "subux.json"; "post.md"; "site.css"; "site.js"; "internal.css";
        "external.css"; "blocked.pipe"; "output.log" ];
    Sys.remove outside;
    Unix.rmdir input
  in
  let results = Fun.protect ~finally:cleanup (fun () ->
    write "post.md" "# Asset fixture\n";
    write "site.css" "/* regular stylesheet */";
    write "site.js" "/* regular module */";
    let run key value expected label =
      write "subux.json" (Json_parser.to_string (Json_parser.Object [ key, value ]));
      let output = Unix.openfile (path "output.log") [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
      let pid = Unix.create_process cli
          [| cli; "check"; "-i"; input; "--tokens"; "../design/tokens.json" |]
          Unix.stdin output output in
      Unix.close output;
      let rec wait remaining =
        let finished, status = Unix.waitpid [ Unix.WNOHANG ] pid in
        if finished <> 0 then Some status
        else if remaining = 0 then begin
          Unix.kill pid Sys.sigkill;
          ignore (Unix.waitpid [] pid);
          None
        end else begin
          ignore (Unix.select [] [] [] 0.01);
          wait (remaining - 1)
        end
      in
      let status = wait 200 in
      let log = In_channel.with_open_bin (path "output.log") In_channel.input_all in
      (label, key, status, log, expected)
    in
    let posix = Sys.os_type <> "Win32" in
    if posix then begin
      Unix.symlink (path "site.css") (path "internal.css");
      Unix.symlink outside (path "external.css");
      Unix.mkfifo (path "blocked.pipe") 0o600
    end;
    List.concat_map (fun key ->
      [ run key (Json_parser.String (if key = "customCss" then "site.css" else "site.js")) 0 "regular asset";
        run key (Json_parser.Bool true) 1 "invalid asset type";
        run key (Json_parser.String "../outside.css") 1 "asset traversal";
        run key (Json_parser.String outside) 1 "absolute asset path" ]
      @ if posix then
          [ run key (Json_parser.String "internal.css") 0 "internal asset symlink";
            run key (Json_parser.String "external.css") 1 "external asset symlink";
            run key (Json_parser.String "blocked.pipe") 1 "FIFO asset must not block" ]
        else []) [ "customCss"; "customJs" ])
  in
  List.iter (fun (label, key, status, log, expected) ->
    assert_true (status = Some (Unix.WEXITED expected)) (key ^ ": " ^ label ^ "\n" ^ log);
    if expected <> 0 then assert_contains log key (label ^ " identifies the asset field")) results;
  print_endline "PASS: CLI custom asset boundary tests"

let test_lighthouse_tasks () =
  let tokens_json = In_channel.with_open_text "../design/tokens.json" In_channel.input_all in
  let css = match Token_compiler.compile_full_stylesheet tokens_json with
    | Ok css -> css | Error e -> fail (error_to_string e) in
  let minified = Token_compiler.inline_stylesheet css in
  Printf.printf "CSS: %d -> %d bytes (%.1f%% reduction)\n" (String.length css)
    (String.length minified) (100. *. (1. -. float_of_int (String.length minified) /. float_of_int (String.length css)));
  assert_eq_str (Token_compiler.minify_css minified) minified "102 idempotent minification";
  assert_eq_str (Token_compiler.minify_css {|/* comment */ .a :hover, .b { font-family: "Pretendard Variable"; width: calc(100% - 2px); content: "/* literal */ \""; }|})
    {|.a :hover,.b{font-family:"Pretendard Variable";width:calc(100% - 2px);content:"/* literal */ \"";}|}
    "102 preserve descendant selector, math, quoted spaces and escapes";
  assert_eq_str (Token_compiler.minify_css {|a { background: url(https://example.com/a/*/image.svg); content: "/* keep */"; }|})
    {|a{background:url(https://example.com/a/*/image.svg);content:"/* keep */";}|}
    "102 preserve comment-looking URL and string bytes";
  assert_contains (Markdown.render ~heading_base:2 "# main-content").html
    "id=\"main-content-1\"" "402 reserve template IDs";
  let standalone = Markdown.render "## Section\n\n#### Subsection\n\n### Included in TOC" in
  assert_contains standalone.html "<h2 id=\"section\">" "402 standalone headings retain source levels";
  assert_contains standalone.html "<h4 id=\"subsection\">" "402 standalone render preserves CommonMark hierarchy";
  assert_true (List.map (fun (t : toc_item) -> t.level) standalone.toc = [2; 3])
    "402 standalone TOC retains the existing h2/h3 contract";
  let critical = Token_compiler.critical_stylesheet
    ~html:"<main class=\"article-content\"><p><a href=\"/\">link</a></p><div id=\"jt-search-modal\"><ul class=\"jt-sidebar-list\"></ul></div></main>" css in
  assert_not_contains critical ".jt-navbar{" "102 remove unrelated page rules";
  assert_contains critical ".jt-sidebar-link{" "102 retain dynamically inserted search links";
  assert_contains critical "light-dark(" "102 fold repeated theme colors";
  assert_true (String.length critical * 100 < String.length css * 50) "102 critical CSS >50% reduction";
  let footnote = Markdown.render ~heading_base:2 "Text[^1]\n\n[^1]: # Footnote heading" in
  assert_not_contains footnote.html "<h1" "402 footnote headings normalized";
  assert_contains footnote.html "id=\"footnote-label\"" "402 footnote description target";
  assert_not_contains css "@import" "101 no render-blocking imports";
  assert_contains css "--sx-color-code-comment: #57606a" "401 accessible code comment token";
  assert_not_contains css ".callout-title { color: var(--sx-color-callout-" "401 no low contrast title";
  assert_contains css ".jt-theme-toggle::before { content: \"◐\"; }"
    "default theme switch keeps its visible icon";
  List.iter (fun cls -> assert_contains css ("." ^ cls) ("302 styled class " ^ cls))
    ["jt-navbar"; "jt-navbar-inner"; "jt-brand"; "jt-nav-links"; "jt-nav-link";
     "jt-search-btn"; "jt-theme-toggle"; "jt-search-modal"; "jt-search-dialog";
     "jt-search-input"; "jt-sidebar-list"; "jt-sidebar"; "jt-sidebar-group";
     "jt-sidebar-title"; "jt-sidebar-link"; "jt-outline"; "jt-outline-title";
     "jt-doc-layout"; "jt-doc-main"; "jt-doc-footer"; "jt-pager-link";
     "jt-pager-label"; "jt-pager-title"; "jt-hero"; "jt-hero-name"; "jt-hero-text";
     "jt-hero-tagline"; "jt-hero-actions"; "jt-btn"; "jt-btn-brand"; "jt-btn-alt";
     "jt-features"; "jt-feature-card"];
  List.iter (fun rule -> assert_contains css rule ("303 metric fallback " ^ rule))
    ["@font-face"; "Subux Gothic Fallback"; "size-adjust:"; "ascent-override:";
     "descent-override:"; "local(\"Apple SD Gothic Neo\")"; "local(\"Malgun Gothic\")"];
  assert_eq_str (Site_builder.content_hash "") "84222325" "103 FNV-1a empty vector";
  assert_eq_str (Site_builder.content_hash "hello") "80aabd0b" "103 FNV-1a hello vector";
  let md = Markdown.render ~heading_base:2 "# Title\n\n#### Jump\n\n## Reset\n\n> ##### Quote\n\n- ###### List\n\n![image](/image.png \"800x450\")\n\n> [!warning]- Warning\n> hidden body" in
  assert_not_contains md.html "<h1" "402 body h1 demoted";
  assert_contains md.html "<h2 id=\"title\">" "402 first heading h2";
  assert_contains md.html "<h3 id=\"jump\">" "402 forward jump normalized";
  assert_contains md.html "<h3 id=\"quote\">" "402 nested quote normalized";
  assert_contains md.html "<h4 id=\"list\">" "402 nested list normalized";
  assert_contains md.html "loading=\"lazy\" decoding=\"async\" width=\"800\" height=\"450\"" "301 image dimensions";
  assert_contains md.html "<details class=\"callout-warning\"" "203 native folding";
  assert_contains md.html "<summary class=\"callout-title\"" "203 keyboard operable summary";
  assert_not_contains md.html " open" "203 minus is initially closed";
  assert_contains (Markdown.render "> [!tip]+ Tip\n> body").html " open>" "203 plus is initially open";
  assert_not_contains (Markdown.render "![bad](/image.png \"800x-1\")").html " width=" "301 invalid dimensions omitted";
  let source : Site_builder.source_file = {collection="articles"; rel_path="articles/hello.md";
    raw_markdown="---\ntitle: Safe title\ndate: 2026-10-28\n---\n# Body\n\nText\n\n```mermaid\ngraph LR; A-->B\n```"} in
  let compile config sources = match Site_builder.compile_site ~config ~tokens_json sources with
    | Ok p -> p | Error e -> fail (error_to_string e) in
  let find (plan : Site_builder.build_plan) path =
    (List.find (fun (a : Site_builder.output_artifact) -> a.out_path = path) plan.artifacts).content in
  let anchored_entry extra =
    match Site_builder.parse_source_file {source with raw_markdown=
      "---\ntitle: Page title\n" ^ extra ^ "---\n# **Page title**\n\n[Back to title](#page-title)\n\n## Page title"} with
    | Ok entry -> entry | Error e -> fail (error_to_string e) in
  let anchored = anchored_entry "" in
  let hero = anchored_entry "layout: home\nhero:\n  name: Landing title\n" in
  List.iter (fun (html, heading) ->
      assert_contains html heading "template H1 preserves the removed Markdown title anchor";
      assert_contains html "href=\"#page-title\"" "title fragment links remain intact";
      assert_contains html "<h2 id=\"page-title-1\">" "body heading does not reuse the title ID")
    [Site_builder.render_blog_article ~config:default_blog_config ~series:None anchored,
       "<h1 id=\"page-title\">";
     Site_builder.render_docs_page ~config:default_docs_config ~all_docs:[anchored] anchored,
       "<h1 id=\"page-title\">";
     Site_builder.render_docs_home ~config:default_docs_config anchored,
       "<h1 class=\"jt-hero-name\" id=\"page-title\">";
     Site_builder.render_docs_home ~config:default_docs_config hero,
       "<h1 class=\"jt-hero-name\" id=\"page-title\">"];
  assert_contains css ".jt-outline a {\n  display: block;\n  padding: 4px var(--sx-space-2);\n  color: var(--sx-color-link);\n  text-decoration: underline;"
    "default outline uses the link token with a non-color indicator";
  let docs_html = find (compile default_docs_config
      [{source with collection="contents"; rel_path="contents/guide.md"}]) "contents/guide/index.html" in
  assert_contains css "a.jt-nav-link[aria-current='page'] {\n  font-weight: 600;\n}"
    "active navbar link includes a non-color weight indicator";
  assert_contains docs_html "if (event.target.closest('a[href]')) closeMenu();"
    "mobile navigation closes when a link is activated";
  assert_contains docs_html "window.addEventListener('hashchange', closeMenu);"
    "mobile navigation closes on same-document hash navigation";
  assert_contains docs_html ".jt-navbar:has(" "critical CSS retains the open-menu header override";
  assert_contains docs_html "overflow-y:auto" "critical CSS retains scrollable dialog rules";
  assert_contains css "max-height: calc(100dvh - 7rem - var(--sx-space-2)); overflow-y: auto"
    "mobile outline uses token spacing and scrolls within the dynamic viewport";
  assert_contains docs_html "max-height:calc(100vh - 7rem - var("
    "critical CSS retains the outline viewport fallback";
  assert_contains docs_html "max-height:calc(100dvh - 7rem - var("
    "critical CSS retains the dynamic viewport constraint with optimized token aliases";
  let collision_entry = anchored_entry "" in
  let collision_entry = {collision_entry with rendered=Markdown.render ~heading_base:2
      "## jt-outline\n\n## jt-outline-toggle\n\n## jt-main-nav"} in
  let collision_html = Site_builder.render_docs_page ~config:default_docs_config
      ~all_docs:[collision_entry] collision_entry in
  assert_contains collision_html "<aside id=\"jt-outline\"" "outline retains its template ID";
  List.iter (fun id ->
    assert_contains collision_html ("<h2 id=\"" ^ id ^ "-1\">")
      "docs headings cannot shadow runtime control lookups";
    assert_contains collision_html ("href=\"#" ^ id ^ "-1\"")
      "outline links point at the disambiguated heading")
    ["jt-outline"; "jt-outline-toggle"; "jt-main-nav"];
  List.iter (fun mode ->
    let config = {(if mode = Docs_mode then default_docs_config else default_blog_config) with mode} in
    let plan = compile config [source] in
    List.iter (fun (a : Site_builder.output_artifact) ->
      if String.ends_with ~suffix:".html" a.out_path then begin
        let rec count_h1 i count =
          if i + 3 >= String.length a.content then count
          else count_h1 (i + 1) (if String.sub a.content i 3 = "<h1" then count + 1 else count) in
        assert_true (count_h1 0 0 = 1) "402 exactly one h1 per template";
        assert_contains a.content "data-sx-inline=\"true\"" "102 inline compiled styles on every page";
        assert_contains a.content "rel=\"preconnect\"" "101 font preconnect";
        assert_contains a.content "media=\"print\"" "101 non-blocking font stylesheet";
        assert_contains a.content "<noscript>" "101 no-JS font fallback";
        assert_contains a.content "<body><a href=\"#main-content\" class=\"skip-link\">" "402 first child skip link";
        assert_contains a.content "id=\"main-content\"" "402 skip target";
        assert_contains a.content "prefers-color-scheme: light" "403 light theme-color";
        assert_contains a.content "prefers-color-scheme: dark" "403 dark theme-color";
        assert_contains a.content "property=\"og:title\"" "403 OpenGraph";
        assert_contains a.content "name=\"twitter:card\"" "403 Twitter card";
        assert_not_contains a.content "href=\"/assets/subux-theme.css\"" "102 no blocking theme request"
      end) plan.artifacts;
    assert_contains (find plan "_headers") "immutable" "103 immutable fingerprint cache";
    assert_contains (find plan "_headers") "nosniff" "103 security headers";
    assert_contains (find plan "robots.txt") "/sitemap.xml" "403 robots sitemap";
    let external_plan = compile {config with inline_css=false} [source] in
    let href = "/assets/subux-theme." ^ Site_builder.content_hash (Token_compiler.minify_css css) ^ ".css" in
    assert_contains (find external_plan "index.html") ("href=\"" ^ href ^ "\"") "103 fingerprint reference";
    assert_eq_str (find external_plan (String.sub href 1 (String.length href - 1))) (Token_compiler.minify_css css) "103 hashed CSS artifact"
  ) [Blog_mode; Docs_mode; Unified_mode];
  let plan = compile default_blog_config [source] in
  let article = find plan "hello/index.html" in
  assert_contains article "application/ld+json" "403 structured article";
  assert_contains article "\"datePublished\":\"2026-10-28\"" "403 article date";
  assert_contains article "IntersectionObserver" "201 viewport-lazy Mermaid";
  assert_contains article "rootMargin:'200px'" "201 viewport proximity";
  assert_contains article "observer.disconnect()" "201 disconnect before import";
  assert_contains article "getComputedStyle(probe).color" "201 resolve light-dark colors before Mermaid";
  assert_contains article "primaryColor:color('active-fill')" "201 Mermaid follows primary or accent theme";
  let unified_source = {source with collection="contents"; rel_path="contents/guide.md"} in
  let unified_plan = compile {default_blog_config with mode=Unified_mode} [source; unified_source] in
  assert_contains (find unified_plan "contents/guide/index.html") "jt-doc-main" "302 unified doc routes preserve contents prefix";
  assert_contains (find unified_plan "tech/index.html") "data-tab-panel=\"tech\"" "202 unified static tabs";
  assert_not_contains (find (compile {default_blog_config with enable_search=false} [source]) "index.html") "id=\"jt-search-open\"" "204 search can be disabled";
  assert_contains (find plan "index.html") "fontDisplay='optional'" "303 prevent late font swap";
  assert_contains (find plan "index.html") "history.pushState" "202 synchronous URL-preserving tabs";
  assert_contains (find plan "tech/index.html") "data-tab-panel=\"tech\"" "202 no-JS static tab";
  let hostile = {source with raw_markdown="---\ntitle: '</script><script>alert(1)</script>'\n---\nBody"} in
  let hostile_html = find (compile default_blog_config [hostile]) "hello/index.html" in
  assert_not_contains hostile_html "</script><script>alert(1)" "403 JSON-LD raw-text injection blocked";
  assert_contains hostile_html "\\u003c/script>" "403 JSON-LD less-than escaped";
  let long_sources = List.init 120 (fun i -> {source with
    rel_path=Printf.sprintf "articles/post-%d.md" i;
    raw_markdown="---\ntitle: 검색 제목\n---\n## 한글 검색\n\n" ^ String.concat "\n\t " (List.init 1000 (fun _ -> {|한글 😊 \"|}))}) in
  let large = find (compile default_blog_config long_sources) "search-index.json" in
  assert_true (String.length large < 20000) "204 120-entry UTF-8 search index <20KB";
  assert_true (String.is_valid_utf_8 large) "204 UTF-8 prefixes remain valid";
  begin match Json_parser.parse large with
  | Ok (Json_parser.Array items) -> assert_true (List.length items = 120) "204 all entries retained"
  | Ok _ | Error _ -> fail "204 valid compact JSON"
  end;
  assert_true (String.length minified * 100 <= String.length css * 70) "102 >=30% inline CSS reduction";
  print_endline "PASS: Lighthouse task regressions"

let () =
  test_frontmatter ();
  test_markdown_rendering ();
  test_safe_urls ();
  test_color_tokens_and_site_builder ();
  test_adapters ();
  test_cli_assets ();

  test_lighthouse_tasks ();
  if Array.length Sys.argv <> 3 then fail "Expected CLI executable and tokens path";
  Cloudflare_cli.run ~executable:Sys.argv.(1) ~tokens_path:Sys.argv.(2);
  print_endline "\nALL TESTS PASSED!"
