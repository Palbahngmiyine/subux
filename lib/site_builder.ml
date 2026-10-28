open Domain

type source_file = {
  collection : string;
  rel_path : string;
  raw_markdown : string;
}

type parsed_entry = {
  collection : string;
  rel_path : string;
  slug : Slug.t;
  title : string;
  meta : frontmatter_meta;
  rendered : Markdown.rendered_markdown;
}

type output_artifact = {
  out_path : string;
  content : string;
}

type build_plan = {
  entries : parsed_entry list;
  artifacts : output_artifact list;
}

module Slug_set = Set.Make (Slug)
module Int_map = Map.Make (Int)

let first_h1_from_markdown (md : string) : string option =
  let lines = String.split_on_char '\n' md in
  let rec loop = function
    | [] -> None
    | line :: rest ->
        let t = String.trim line in
        if String.starts_with ~prefix:"# " t then
          let h = String.trim (String.sub t 2 (String.length t - 2)) in
          if h <> "" then Some h else loop rest
        else loop rest
  in
  loop lines

let fallback_title_of_slug (slug : Slug.t) : string =
  let s = Slug.to_string slug in
  if s = "" then "Home"
  else
    match List.rev (String.split_on_char '/' s) with
    | last :: _ when last <> "" -> last
    | _ -> s

let parse_source_file (src : source_file) : (parsed_entry, build_error) result =
  let open Result_syntax in
  let* doc =
    Frontmatter.parse ~file:src.rel_path src.raw_markdown
  in
  let slug = Slug.of_key ~base_folder:src.collection src.rel_path in
  let rendered = Markdown.render doc.body in
  let title =
    match doc.meta.title with
    | Some t when String.trim t <> "" -> String.trim t
    | _ ->
        begin match doc.meta.hero with
        | Some h when String.trim h.name <> "" -> String.trim h.name
        | _ ->
            begin match first_h1_from_markdown doc.body with
            | Some h1 -> h1
            | None -> fallback_title_of_slug slug
            end
        end
  in
  Ok
    {
      collection = src.collection;
      rel_path = src.rel_path;
      slug;
      title;
      meta = doc.meta;
      rendered;
    }

let compare_entries (a : parsed_entry) (b : parsed_entry) : int =
  let fallback () =
    let tc = String.compare a.title b.title in
    if tc <> 0 then tc else String.compare a.rel_path b.rel_path
  in
  match (a.meta.date, b.meta.date) with
  | None, None -> fallback ()
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some da, Some db ->
      let dc = compare_date_desc da db in
      if dc <> 0 then dc
      else
        begin match (a.meta.series, b.meta.series, a.meta.series_order, b.meta.series_order) with
        | Some sa, Some sb, Some oa, Some ob when sa = sb && oa <> ob ->
            Int.compare ob oa
        | _ -> fallback ()
        end

let sort_entries (entries : parsed_entry list) : parsed_entry list =
  List.sort compare_entries entries

let group_entries_by_year (entries : parsed_entry list) :
    (int * parsed_entry list) list =
  let sorted = sort_entries entries in
  let grouped_map =
    List.fold_left
      (fun acc entry ->
        let year =
          match entry.meta.date with
          | Some d -> d.year
          | None -> 2026
        in
        let prev = Option.value ~default:[] (Int_map.find_opt year acc) in
        Int_map.add year (prev @ [ entry ]) acc)
      Int_map.empty sorted
  in
  Int_map.bindings grouped_map
  |> List.sort (fun (y1, _) (y2, _) -> Int.compare y2 y1)

let compare_date_asc_opt (da : date_val option) (db : date_val option) : int =
  match (da, db) with
  | Some a, Some b -> compare_date_desc b a
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let build_series_info ~(series_name : string) ~(current_slug : Slug.t)
    (all_entries : parsed_entry list) : series_info option =
  let target = String.trim series_name in
  if target = "" then None
  else
    let _, matching_rev =
      List.fold_left
        (fun (seen, acc) entry ->
          match entry.meta.series with
          | Some s when String.trim s = target ->
              if Slug_set.mem entry.slug seen then (seen, acc)
              else (Slug_set.add entry.slug seen, entry :: acc)
          | _ -> (seen, acc))
        (Slug_set.empty, []) all_entries
    in
    let matching = List.rev matching_rev in
    if List.length matching < 2 then None
    else
      let sorted =
        List.sort
          (fun a b ->
            match (a.meta.series_order, b.meta.series_order) with
            | Some oa, Some ob ->
                let c = Int.compare oa ob in
                if c <> 0 then c else compare_date_asc_opt a.meta.date b.meta.date
            | Some _, None -> -1
            | None, Some _ -> 1
            | None, None -> compare_date_asc_opt a.meta.date b.meta.date)
          matching
      in
      let items =
        List.map
          (fun (entry : parsed_entry) : series_item ->
            {
              slug = entry.slug;
              title = entry.title;
              is_current = Slug.equal entry.slug current_slug;
            })
          sorted
      in
      Some { name = target; items }

(* Shared HTML Head & Scripts *)
let github_svg_node : Html.node =
  Html.raw
    "<svg viewBox=\"0 0 16 16\" fill=\"currentColor\" aria-hidden=\"true\" width=\"24\" height=\"24\"><path d=\"M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z\"/></svg>"

let common_head ~(page_title : string) ~(description : string)
    ~(canonical_path : string) ~(config : site_config) : Html.node list =
  let canonical_url =
    let base =
      if String.ends_with ~suffix:"/" config.base_url then
        String.sub config.base_url 0 (String.length config.base_url - 1)
      else config.base_url
    in
    base ^ canonical_path
  in
  [
    Html.void "meta" ~attrs:[ ("charset", "utf-8") ] ();
    Html.el "title" [ Html.txt page_title ];
    Html.void "meta"
      ~attrs:
        [
          ("name", "viewport");
          ("content", "width=device-width, initial-scale=1.0");
        ]
      ();
    Html.void "meta"
      ~attrs:[ ("name", "description"); ("content", description) ]
      ();
    Html.void "meta"
      ~attrs:[ ("name", "theme-color"); ("content", "#c2e7ff") ]
      ();
    Html.void "link"
      ~attrs:[ ("rel", "canonical"); ("href", canonical_url) ]
      ();
    Html.void "link"
      ~attrs:[ ("rel", "stylesheet"); ("href", "/assets/subux-theme.css") ]
      ();
  ]

let mermaid_runtime_script : Html.node =
  Html.el "script"
    ~attrs:[ ("type", "module") ]
    [
      Html.raw
        "const blocks = Array.from(document.querySelectorAll('.article-content pre > code.language-mermaid'));\n\
         if (blocks.length > 0) {\n\
           import('https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs').then(async ({ default: mermaid }) => {\n\
             mermaid.initialize({\n\
               startOnLoad: false,\n\
               securityLevel: 'strict',\n\
               theme: 'base',\n\
               layout: 'dagre',\n\
               look: 'classic',\n\
               themeVariables: {\n\
                 background: '#ffffff',\n\
                 primaryColor: '#eff6ff',\n\
                 primaryBorderColor: '#2563eb',\n\
                 primaryTextColor: '#0f172a',\n\
                 secondaryColor: '#ecfdf5',\n\
                 secondaryBorderColor: '#059669',\n\
                 tertiaryColor: '#fff7ed',\n\
                 tertiaryBorderColor: '#f97316',\n\
                 lineColor: '#2563eb'\n\
               }\n\
             });\n\
             const nodes = [];\n\
             for (const block of blocks) {\n\
               const pre = block.parentElement;\n\
               if (!pre || pre.dataset.mermaidProcessed === 'true') continue;\n\
               const diagram = document.createElement('div');\n\
               diagram.className = 'mermaid';\n\
               diagram.textContent = block.textContent ?? '';\n\
               pre.dataset.mermaidProcessed = 'true';\n\
               pre.replaceWith(diagram);\n\
               nodes.push(diagram);\n\
             }\n\
             if (nodes.length > 0) await mermaid.run({ nodes });\n\
           });\n\
         }";
    ]

let subux_tabs_runtime_script : Html.node =
  Html.el "script"
    [
      Html.raw
        "(function(){\n\
           const params = new URLSearchParams(window.location.search);\n\
           const tab = params.get('tab');\n\
           if (!tab) return;\n\
           const panels = document.querySelectorAll('[data-tab-panel]');\n\
           const links = document.querySelectorAll('a.tab-link[data-tab-id]');\n\
           let matched = false;\n\
           panels.forEach(p => { if (p.getAttribute('data-tab-panel') === tab) matched = true; });\n\
           if (!matched) return;\n\
           panels.forEach(p => {\n\
             p.hidden = p.getAttribute('data-tab-panel') !== tab;\n\
           });\n\
           links.forEach(l => {\n\
             if (l.getAttribute('data-tab-id') === tab) {\n\
               l.setAttribute('aria-current', 'page');\n\
             } else {\n\
               l.removeAttribute('aria-current');\n\
             }\n\
           });\n\
         })();";
    ]

(* 1. Blog_mode Home, Article & 404 Renderers *)
let render_article_list_node (entries : parsed_entry list) : Html.node =
  if entries = [] then
    Html.el "div"
      ~attrs:[ ("class", "empty-state") ]
      [ Html.el "p" [ Html.txt "No articles yet. Will be updated soon..." ] ]
  else
    let groups = group_entries_by_year entries in
    Html.el "nav"
      ~attrs:[ ("class", "articles") ]
      (List.map
         (fun (year, year_entries) ->
           let year_str = Int.to_string year in
           Html.el "div"
             [
               Html.el "h2"
                 [
                   Html.el "time"
                     ~attrs:[ ("datetime", year_str) ]
                     [ Html.txt (year_str ^ "년") ];
                 ];
               Html.el "ul"
                 (List.map
                    (fun entry ->
                      let date_node =
                        match entry.meta.date with
                        | None -> Html.empty
                        | Some d ->
                            Html.el "time"
                              ~attrs:[ ("datetime", format_iso d) ]
                              [ Html.txt (format_korean_short d) ]
                      in
                      Html.el "li"
                        [
                          date_node;
                          Html.el "a"
                            ~attrs:
                              [
                                ("href", Slug.to_href entry.slug);
                                ("class", "article-link");
                              ]
                            [ Html.txt entry.title ];
                        ])
                    year_entries);
             ])
         groups)

let render_blog_home ~(config : site_config) ~(active_tab : string)
    (all_entries : parsed_entry list) : string =
  let tabs =
    if config.tabs = [] then default_blog_config.tabs else config.tabs
  in
  let github_node =
    match config.github_url with
    | None -> Html.empty
    | Some gh_url ->
        Html.el "a"
          ~attrs:
            [
              ("href", gh_url);
              ("target", "_blank");
              ("rel", "noopener noreferrer");
              ("class", "github-link");
              ("title", "GitHub");
            ]
          [
            Html.el "span" ~attrs:[ ("class", "sr-only") ] [ Html.txt "GitHub" ];
            github_svg_node;
          ]
  in
  let nav_links =
    List.map
      (fun (t : collection_tab) ->
        let is_active = t.id = active_tab in
        let base_attrs =
          [
            ("href", "/?tab=" ^ t.id);
            ("class", "tab-link");
            ("data-tab-id", t.id);
          ]
        in
        let attrs =
          if is_active then base_attrs @ [ ("aria-current", "page") ]
          else base_attrs
        in
        Html.el "a" ~attrs [ Html.txt t.label ])
      tabs
  in
  let tab_panels =
    List.map
      (fun (t : collection_tab) ->
        let tab_entries =
          List.filter
            (fun (e : parsed_entry) ->
              (not e.meta.draft)
              && (e.collection = t.folder || e.collection = t.id))
            all_entries
        in
        let disclaimer_node =
          match t.disclaimer with
          | None -> Html.empty
          | Some text ->
              Html.el "div"
                ~attrs:[ ("class", "disclaimer-banner") ]
                [ Html.el "p" [ Html.txt text ] ]
        in
        let panel_attrs =
          if t.id = active_tab then [ ("data-tab-panel", t.id) ]
          else [ ("data-tab-panel", t.id); ("hidden", "hidden") ]
        in
        Html.el "div" ~attrs:panel_attrs
          [ disclaimer_node; render_article_list_node tab_entries ])
      tabs
  in
  let page_title =
    if config.description <> "" then config.description ^ " | " ^ config.title
    else config.title
  in
  let head =
    common_head ~page_title ~description:config.description ~canonical_path:"/"
      ~config
  in
  let body =
    [
      Html.el "main"
        ~attrs:[ ("class", "site-container") ]
        [
          Html.el "div"
            ~attrs:[ ("class", "page-wrapper") ]
            [
              Html.el "header"
                ~attrs:[ ("class", "site-header") ]
                [ Html.el "h1" [ Html.txt config.title ]; github_node ];
              Html.el "div"
                ~attrs:[ ("class", "tabs-container") ]
                [
                  Html.el "nav"
                    ~attrs:[ ("class", "tabs-nav"); ("aria-label", "Tabs") ]
                    nav_links;
                  Html.el "div" ~attrs:[ ("class", "tabs-content") ] tab_panels;
                ];
            ];
        ];
      subux_tabs_runtime_script;
    ]
  in
  Html.to_document ~lang:config.lang ~head ~body

let render_blog_article ~(config : site_config)
    ~(series : series_info option) (entry : parsed_entry) : string =
  let date_node =
    match entry.meta.date with
    | None -> Html.empty
    | Some d ->
        Html.el "time"
          ~attrs:[ ("datetime", format_iso d) ]
          [ Html.txt (format_korean_full d) ]
  in
  let series_node =
    match series with
    | None -> Html.empty
    | Some s ->
        Html.el "aside"
          ~attrs:[ ("class", "series-nav-box"); ("aria-label", "시리즈 목차") ]
          [
            Html.el "p"
              ~attrs:[ ("class", "series-nav-label") ]
              [ Html.txt ("시리즈 · " ^ s.name) ];
            Html.el "ol"
              ~attrs:[ ("class", "series-nav-list") ]
              (List.map
                 (fun (item : series_item) ->
                   if item.is_current then
                     Html.el "li"
                       [
                         Html.el "span"
                           ~attrs:
                             [
                               ("class", "series-nav-current");
                               ("aria-current", "page");
                             ]
                           [ Html.txt item.title ];
                       ]
                   else
                     Html.el "li"
                       [
                         Html.el "a"
                           ~attrs:
                             [
                               ("href", Slug.to_href item.slug);
                               ("class", "series-nav-link");
                             ]
                           [ Html.txt item.title ];
                       ])
                 s.items);
          ]
  in
  let desc =
    match entry.meta.description with
    | Some d when d <> "" -> d
    | _ -> config.description
  in
  let head =
    common_head ~page_title:entry.title ~description:desc
      ~canonical_path:(Slug.to_href entry.slug)
      ~config
  in
  let scripts =
    if entry.rendered.has_mermaid then [ mermaid_runtime_script ] else []
  in
  let body =
    [
      Html.el "main"
        ~attrs:[ ("class", "site-container") ]
        [
          Html.el "section"
            ~attrs:[ ("class", "content-wrapper") ]
            [
              Html.el "nav"
                ~attrs:[ ("class", "article-nav") ]
                [
                  Html.el "a"
                    ~attrs:[ ("href", "/"); ("class", "back-link") ]
                    [ Html.txt "← 처음으로" ];
                ];
              Html.el "article"
                [
                  Html.el "header"
                    ~attrs:[ ("class", "article-header") ]
                    [ Html.el "h1" [ Html.txt entry.title ]; date_node ];
                  series_node;
                  Html.el "div"
                    ~attrs:[ ("class", "article-content") ]
                    [ Html.raw entry.rendered.html ];
                ];
            ];
        ]
    ]
    @ scripts
  in
  Html.to_document ~lang:config.lang ~head ~body

let render_not_found ~(config : site_config) : string =
  let page_title = "이곳엔 아무것도 없습니다 | " ^ config.title in
  let head =
    common_head ~page_title ~description:config.description
      ~canonical_path:"/404.html" ~config
  in
  let body =
    [
      Html.el "main"
        ~attrs:[ ("class", "site-container") ]
        [
          Html.el "div"
            ~attrs:[ ("class", "not-found-page") ]
            [
              Html.el "h1"
                ~attrs:[ ("class", "not-found-title") ]
                [ Html.txt "이곳엔 아무것도 없습니다." ];
              Html.el "a"
                ~attrs:[ ("href", "/"); ("class", "not-found-link") ]
                [ Html.txt "처음으로" ];
            ];
        ];
    ]
  in
  Html.to_document ~lang:config.lang ~head ~body

(* 2. Docs_mode Navbar, Search Modal, Home & Documentation Page Renderers *)
let jt_runtime_script : Html.node =
  Html.el "script"
    [
      Html.raw
        "(function(){\n\
           const root = document.documentElement;\n\
           const saved = localStorage.getItem('sx-theme');\n\
           if (saved === 'light' || saved === 'dark') root.setAttribute('data-theme', saved);\n\
           const themeBtn = document.getElementById('jt-theme-toggle');\n\
           if (themeBtn) {\n\
             themeBtn.addEventListener('click', () => {\n\
               const cur = root.getAttribute('data-theme');\n\
               const next = cur === 'dark' ? 'light' : 'dark';\n\
               root.setAttribute('data-theme', next);\n\
               localStorage.setItem('sx-theme', next);\n\
             });\n\
           }\n\
           const modal = document.getElementById('jt-search-modal');\n\
           const openBtn = document.getElementById('jt-search-open');\n\
           const input = document.getElementById('jt-search-input');\n\
           const results = document.getElementById('jt-search-results');\n\
           let indexData = null;\n\
           async function openSearch() {\n\
             if (!modal) return;\n\
             modal.hidden = false;\n\
             if (input) input.focus();\n\
             if (!indexData) {\n\
               try {\n\
                 const res = await fetch('/search-index.json');\n\
                 indexData = await res.json();\n\
               } catch { indexData = []; }\n\
             }\n\
           }\n\
           function closeSearch() { if (modal) modal.hidden = true; }\n\
           if (openBtn) openBtn.addEventListener('click', openSearch);\n\
           window.addEventListener('keydown', (e) => {\n\
             if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === 'k') {\n\
               e.preventDefault();\n\
               if (modal && !modal.hidden) closeSearch(); else openSearch();\n\
             } else if (e.key === 'Escape') closeSearch();\n\
           });\n\
           if (modal) {\n\
             modal.addEventListener('click', (e) => { if (e.target === modal) closeSearch(); });\n\
           }\n\
           if (input && results) {\n\
             input.addEventListener('input', () => {\n\
               const q = input.value.trim().toLowerCase();\n\
               results.replaceChildren();\n\
               if (!q || !Array.isArray(indexData)) return;\n\
               indexData.filter(it =>\n\
                 (it.title && it.title.toLowerCase().includes(q)) ||\n\
                 (it.text && it.text.toLowerCase().includes(q))\n\
               ).slice(0, 8).forEach(it => {\n\
                 const li = document.createElement('li');\n\
                 const a = document.createElement('a');\n\
                 a.href = it.href;\n\
                 a.className = 'jt-sidebar-link';\n\
                 a.textContent = it.title;\n\
                 li.appendChild(a);\n\
                 results.appendChild(li);\n\
               });\n\
             });\n\
           }\n\
         })();";
    ]

let render_jt_navbar (config : site_config) : Html.node =
  let nav_links =
    List.map
      (fun (item : nav_item) ->
        Html.el "a"
          ~attrs:[ ("href", item.link); ("class", "jt-nav-link") ]
          [ Html.txt item.text ])
      config.nav
  in
  let search_btn =
    if config.enable_search then
      Html.el "button"
        ~attrs:
          [
            ("type", "button");
            ("id", "jt-search-open");
            ("class", "jt-search-btn");
            ("aria-label", "Search documentation");
          ]
        [ Html.txt "Search ⌘K" ]
    else Html.empty
  in
  let theme_btn =
    Html.el "button"
      ~attrs:
        [
          ("type", "button");
          ("id", "jt-theme-toggle");
          ("class", "jt-theme-toggle");
          ("aria-label", "Toggle color theme");
        ]
      [ Html.txt "◐" ]
  in
  let github_node =
    match config.github_url with
    | None -> Html.empty
    | Some gh_url ->
        Html.el "a"
          ~attrs:
            [
              ("href", gh_url);
              ("target", "_blank");
              ("rel", "noopener noreferrer");
              ("class", "github-link");
              ("title", "GitHub");
            ]
          [
            Html.el "span" ~attrs:[ ("class", "sr-only") ] [ Html.txt "GitHub" ];
            github_svg_node;
          ]
  in
  Html.el "header"
    ~attrs:[ ("class", "jt-navbar") ]
    [
      Html.el "div"
        ~attrs:[ ("class", "jt-navbar-inner") ]
        [
          Html.el "a"
            ~attrs:[ ("href", "/"); ("class", "jt-brand") ]
            [ Html.txt config.title ];
          Html.el "nav"
            ~attrs:[ ("class", "jt-nav-links"); ("aria-label", "Main Navigation") ]
            (nav_links @ [ search_btn; theme_btn; github_node ]);
        ];
    ]

let render_jt_search_modal (config : site_config) : Html.node =
  if not config.enable_search then Html.empty
  else
    Html.el "div"
      ~attrs:
        [
          ("id", "jt-search-modal");
          ("class", "jt-search-modal");
          ("role", "dialog");
          ("aria-modal", "true");
          ("aria-label", "Search");
          ("hidden", "hidden");
        ]
      [
        Html.el "div"
          ~attrs:[ ("class", "jt-search-dialog") ]
          [
            Html.void "input"
              ~attrs:
                [
                  ("type", "search");
                  ("id", "jt-search-input");
                  ("class", "jt-search-input");
                  ("placeholder", "Search docs...");
                  ("autocomplete", "off");
                ]
              ();
            Html.el "ul"
              ~attrs:
                [ ("id", "jt-search-results"); ("class", "jt-sidebar-list") ]
              [];
          ];
      ]

let normalize_href_for_match (href : string) : string =
  let s = String.trim href in
  let len = String.length s in
  if len > 1 && s.[len - 1] = '/' then String.sub s 0 (len - 1)
  else if String.ends_with ~suffix:".md" s then
    String.sub s 0 (len - 3)
  else s

let effective_sidebar_groups (config : site_config)
    (all_docs : parsed_entry list) : sidebar_group list =
  let doc_entries =
    all_docs
    |> List.filter (fun (e : parsed_entry) ->
           (not e.meta.draft)
           && Slug.to_string e.slug <> ""
           && e.meta.layout <> Some Layout_home)
    |> List.sort (fun a b ->
           match (a.meta.order, b.meta.order) with
           | Some oa, Some ob -> Int.compare oa ob
           | Some _, None -> -1
           | None, Some _ -> 1
           | None, None -> String.compare a.title b.title)
  in
  let auto_items =
    List.map
      (fun (e : parsed_entry) : sidebar_entry ->
        { text = e.title; link = Slug.to_href e.slug })
      doc_entries
  in
  if config.sidebar = [] then
    if auto_items = [] then [] else [ { text = "Contents"; items = auto_items } ]
  else if not config.auto_sidebar then config.sidebar
  else
    let configured_links =
      List.concat_map
        (fun (g : sidebar_group) ->
          List.map (fun (it : sidebar_entry) -> normalize_href_for_match it.link) g.items)
        config.sidebar
    in
    let extra_items =
      List.filter
        (fun (it : sidebar_entry) ->
          not (List.mem (normalize_href_for_match it.link) configured_links))
        auto_items
    in
    if extra_items = [] then config.sidebar
    else config.sidebar @ [ { text = "Contents"; items = extra_items } ]

let find_prev_next_from_sidebar (groups : sidebar_group list)
    (current_href : string) : (string * string) option * (string * string) option =
  let flat = List.concat_map (fun (g : sidebar_group) -> g.items) groups in
  let norm_cur = normalize_href_for_match current_href in
  let rec loop prev = function
    | [] -> (None, None)
    | (item : sidebar_entry) :: rest ->
        if normalize_href_for_match item.link = norm_cur then
          let next =
            match rest with
            | nxt :: _ -> Some (nxt.text, nxt.link)
            | [] -> None
          in
          (prev, next)
        else loop (Some (item.text, item.link)) rest
  in
  loop None flat

let render_docs_home ~(config : site_config) (entry : parsed_entry) :
    string =
  let hero_node =
    match entry.meta.hero with
    | None ->
        Html.el "section"
          ~attrs:[ ("class", "jt-hero") ]
          [
            Html.el "h1"
              ~attrs:[ ("class", "jt-hero-name") ]
              [ Html.txt entry.title ];
          ]
    | Some h ->
        let text_node =
          match h.text with
          | None -> Html.empty
          | Some t ->
              Html.el "p" ~attrs:[ ("class", "jt-hero-text") ] [ Html.txt t ]
        in
        let tagline_node =
          match h.tagline with
          | None -> Html.empty
          | Some tl ->
              Html.el "p"
                ~attrs:[ ("class", "jt-hero-tagline") ]
                [ Html.txt tl ]
        in
        let actions_node =
          if h.actions = [] then Html.empty
          else
            Html.el "div"
              ~attrs:[ ("class", "jt-hero-actions") ]
              (List.map
                 (fun (act : hero_action) ->
                   let cls =
                     match act.theme with
                     | Brand_action -> "jt-btn jt-btn-brand"
                     | Alt_action -> "jt-btn jt-btn-alt"
                   in
                   let safe_href =
                     Option.value ~default:"/"
                       (Safe_url.sanitize_url ~allow_contact:true
                          ~decode_entities:false act.link)
                   in
                   Html.el "a"
                     ~attrs:[ ("href", safe_href); ("class", cls) ]
                     [ Html.txt act.text ])
                 h.actions)
        in
        Html.el "section"
          ~attrs:[ ("class", "jt-hero") ]
          [
            Html.el "h1"
              ~attrs:[ ("class", "jt-hero-name") ]
              [ Html.txt h.name ];
            text_node;
            tagline_node;
            actions_node;
          ]
  in
  let features_node =
    if entry.meta.features = [] then Html.empty
    else
      Html.el "section"
        ~attrs:[ ("class", "jt-features") ]
        (List.map
           (fun (f : feature_card) ->
             Html.el "article"
               ~attrs:[ ("class", "jt-feature-card") ]
               [
                 Html.el "h2" [ Html.txt f.title ];
                 Html.el "p" [ Html.txt f.details ];
               ])
           entry.meta.features)
  in
  let body_md_node =
    if String.trim entry.rendered.html = "" then Html.empty
    else
      Html.el "section"
        ~attrs:[ ("class", "content-wrapper") ]
        [
          Html.el "div"
            ~attrs:[ ("class", "article-content") ]
            [ Html.raw entry.rendered.html ];
        ]
  in
  let head =
    common_head ~page_title:config.title ~description:config.description
      ~canonical_path:"/" ~config
  in
  let body =
    [
      render_jt_navbar config;
      Html.el "main" [ hero_node; features_node; body_md_node ];
      render_jt_search_modal config;
      jt_runtime_script;
    ]
  in
  Html.to_document ~lang:config.lang ~head ~body

let render_docs_page ~(config : site_config)
    ~(all_docs : parsed_entry list) (entry : parsed_entry) : string =
  let current_href = Slug.to_href entry.slug in
  let norm_cur = normalize_href_for_match current_href in
  let groups = effective_sidebar_groups config all_docs in
  let sidebar_node =
    Html.el "aside"
      ~attrs:[ ("class", "jt-sidebar"); ("aria-label", "Sidebar Navigation") ]
      (List.map
         (fun (g : sidebar_group) ->
           Html.el "div"
             ~attrs:[ ("class", "jt-sidebar-group") ]
             [
               Html.el "p"
                 ~attrs:[ ("class", "jt-sidebar-title") ]
                 [ Html.txt g.text ];
               Html.el "ul"
                 ~attrs:[ ("class", "jt-sidebar-list") ]
                 (List.map
                    (fun (item : sidebar_entry) ->
                      let is_active =
                        normalize_href_for_match item.link = norm_cur
                      in
                      let attrs =
                        if is_active then
                          [
                            ("href", item.link);
                            ("class", "jt-sidebar-link active");
                            ("aria-current", "page");
                          ]
                        else
                          [ ("href", item.link); ("class", "jt-sidebar-link") ]
                      in
                      Html.el "li"
                        [ Html.el "a" ~attrs [ Html.txt item.text ] ])
                    g.items);
             ])
         groups)
  in
  let outline_node =
    if entry.rendered.toc = [] then
      Html.el "aside"
        ~attrs:[ ("class", "jt-outline"); ("aria-label", "On this page") ]
        []
    else
      Html.el "aside"
        ~attrs:[ ("class", "jt-outline"); ("aria-label", "On this page") ]
        [
          Html.el "p"
            ~attrs:[ ("class", "jt-outline-title") ]
            [ Html.txt "On this page" ];
          Html.el "ul"
            (List.map
               (fun (t : toc_item) ->
                 Html.el "li"
                   [
                     Html.el "a"
                       ~attrs:[ ("href", "#" ^ t.id) ]
                       [ Html.txt t.text ];
                   ])
               entry.rendered.toc);
        ]
  in
  let auto_prev, auto_next = find_prev_next_from_sidebar groups current_href in
  let prev_opt =
    match entry.meta.prev_link with Some _ as p -> p | None -> auto_prev
  in
  let next_opt =
    match entry.meta.next_link with Some _ as n -> n | None -> auto_next
  in
  let prev_node =
    match prev_opt with
    | None -> Html.el "div" []
    | Some (text, href) ->
        Html.el "a"
          ~attrs:[ ("href", href); ("class", "jt-pager-link prev") ]
          [
            Html.el "span"
              ~attrs:[ ("class", "jt-pager-label") ]
              [ Html.txt "Previous page" ];
            Html.el "span"
              ~attrs:[ ("class", "jt-pager-title") ]
              [ Html.txt text ];
          ]
  in
  let next_node =
    match next_opt with
    | None -> Html.el "div" []
    | Some (text, href) ->
        Html.el "a"
          ~attrs:[ ("href", href); ("class", "jt-pager-link next") ]
          [
            Html.el "span"
              ~attrs:[ ("class", "jt-pager-label") ]
              [ Html.txt "Next page" ];
            Html.el "span"
              ~attrs:[ ("class", "jt-pager-title") ]
              [ Html.txt text ];
          ]
  in
  let pager_footer =
    if prev_opt = None && next_opt = None then Html.empty
    else
      Html.el "nav"
        ~attrs:[ ("class", "jt-doc-footer"); ("aria-label", "Pager") ]
        [ prev_node; next_node ]
  in
  let page_title = entry.title ^ " | " ^ config.title in
  let head =
    common_head ~page_title ~description:config.description
      ~canonical_path:current_href ~config
  in
  let scripts =
    if entry.rendered.has_mermaid then
      [ jt_runtime_script; mermaid_runtime_script ]
    else [ jt_runtime_script ]
  in
  let body =
    [
      render_jt_navbar config;
      Html.el "div"
        ~attrs:[ ("class", "jt-doc-layout") ]
        [
          sidebar_node;
          Html.el "main"
            ~attrs:[ ("class", "jt-doc-main") ]
            [
              Html.el "article"
                ~attrs:[ ("class", "article-content") ]
                [ Html.raw entry.rendered.html ];
              pager_footer;
            ];
          outline_node;
        ];
      render_jt_search_modal config;
    ]
    @ scripts
  in
  Html.to_document ~lang:config.lang ~head ~body

(* 3. Search Index & Sitemap Generators *)
let build_search_index_json (entries : parsed_entry list) : string =
  let items =
    entries
    |> List.filter (fun (e : parsed_entry) -> not e.meta.draft)
    |> List.map (fun (e : parsed_entry) ->
           Json_parser.Object
             [
               ("title", Json_parser.String e.title);
               ("href", Json_parser.String (Slug.to_href e.slug));
               ("collection", Json_parser.String e.collection);
               ("text", Json_parser.String e.rendered.plain_text);
             ])
  in
  Json_parser.to_string (Json_parser.Array items)

let build_sitemap_xml (config : site_config) (entries : parsed_entry list) :
    string =
  let base =
    if String.ends_with ~suffix:"/" config.base_url then
      String.sub config.base_url 0 (String.length config.base_url - 1)
    else config.base_url
  in
  let buf = Buffer.create 2048 in
  Buffer.add_string buf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  Buffer.add_string buf
    "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n";
  Buffer.add_string buf ("  <url><loc>" ^ Html.escape_text (base ^ "/") ^ "</loc></url>\n");
  List.iter
    (fun (e : parsed_entry) ->
      if (not e.meta.draft) && Slug.to_string e.slug <> "" then begin
        let loc = base ^ Slug.to_href e.slug in
        Buffer.add_string buf
          ("  <url><loc>" ^ Html.escape_text loc ^ "</loc></url>\n")
      end)
    entries;
  Buffer.add_string buf "</urlset>\n";
  Buffer.contents buf

let out_path_of_slug (slug : Slug.t) : string =
  let s = Slug.to_string slug in
  if s = "" then "index.html" else s ^ "/index.html"

(* 4. Full Pure Site Compilation Pipeline *)
let compile_site ~(config : site_config) ~(tokens_json : string)
    (sources : source_file list) : (build_plan, build_error) result =
  let open Result_syntax in
  let* css_bundle =
    Token_compiler.compile_full_stylesheet ~area_tone:config.area_tone tokens_json
  in
  let* entries = map_list parse_source_file sources in
  let published = List.filter (fun (e : parsed_entry) -> not e.meta.draft) entries in
  let page_artifacts =
    match config.mode with
    | Blog_mode ->
        let tabs =
          if config.tabs = [] then default_blog_config.tabs else config.tabs
        in
        let first_tab_id =
          match tabs with t :: _ -> t.id | [] -> "articles"
        in
        let home_html =
          render_blog_home ~config ~active_tab:first_tab_id published
        in
        let tab_pages =
          List.map
            (fun (t : collection_tab) ->
              {
                out_path = t.id ^ "/index.html";
                content = render_blog_home ~config ~active_tab:t.id published;
              })
            tabs
        in
        let article_pages =
          List.map
            (fun (e : parsed_entry) ->
              let series =
                match e.meta.series with
                | None -> None
                | Some s_name ->
                    build_series_info ~series_name:s_name ~current_slug:e.slug
                      published
              in
              {
                out_path = out_path_of_slug e.slug;
                content = render_blog_article ~config ~series e;
              })
            published
        in
        ({ out_path = "index.html"; content = home_html } :: tab_pages)
        @ article_pages
    | Docs_mode ->
        let home_entry_opt =
          List.find_opt
            (fun (e : parsed_entry) ->
              e.meta.layout = Some Layout_home || Slug.to_string e.slug = "")
            published
        in
        let home_artifact =
          match home_entry_opt with
          | Some home_entry ->
              {
                out_path = "index.html";
                content = render_docs_home ~config home_entry;
              }
          | None ->
              let synthetic_home =
                {
                  collection = "";
                  rel_path = "index.md";
                  slug = Slug.of_string "";
                  title = config.title;
                  meta =
                    {
                      empty_frontmatter with
                      layout = Some Layout_home;
                      hero =
                        Some
                          {
                            name = config.title;
                            text = Some config.description;
                            tagline = None;
                            actions = [];
                          };
                    };
                  rendered = Markdown.render "";
                }
              in
              {
                out_path = "index.html";
                content = render_docs_home ~config synthetic_home;
              }
        in
        let doc_pages =
          published
          |> List.filter (fun (e : parsed_entry) ->
                 Slug.to_string e.slug <> "" && e.meta.layout <> Some Layout_home)
          |> List.map (fun (e : parsed_entry) ->
                 {
                   out_path = out_path_of_slug e.slug;
                   content =
                     render_docs_page ~config ~all_docs:published e;
                 })
        in
        home_artifact :: doc_pages
    | Unified_mode ->
        let tabs =
          if config.tabs = [] then default_blog_config.tabs else config.tabs
        in
        let first_tab_id =
          match tabs with t :: _ -> t.id | [] -> "articles"
        in
        let home_html =
          render_blog_home ~config ~active_tab:first_tab_id published
        in
        let rendered_pages =
          List.filter_map
            (fun (e : parsed_entry) ->
              if Slug.to_string e.slug = "" && e.meta.layout = Some Layout_home
              then
                Some
                  {
                    out_path = "index.html";
                    content = render_docs_home ~config e;
                  }
              else if Slug.to_string e.slug = "" then None
              else if e.meta.layout = Some Layout_doc || e.collection = "contents" || e.collection = "docs"
              then
                Some
                  {
                    out_path = out_path_of_slug e.slug;
                    content =
                      render_docs_page ~config ~all_docs:published e;
                  }
              else
                let series =
                  match e.meta.series with
                  | None -> None
                  | Some s_name ->
                      build_series_info ~series_name:s_name
                        ~current_slug:e.slug published
                in
                Some
                  {
                    out_path = out_path_of_slug e.slug;
                    content = render_blog_article ~config ~series e;
                  })
            published
        in
        if List.exists (fun a -> a.out_path = "index.html") rendered_pages then
          rendered_pages
        else { out_path = "index.html"; content = home_html } :: rendered_pages
  in
  let system_artifacts =
    [
      { out_path = "assets/subux-theme.css"; content = css_bundle };
      { out_path = "404.html"; content = render_not_found ~config };
      {
        out_path = "search-index.json";
        content = build_search_index_json published;
      };
      {
        out_path = "sitemap.xml";
        content = build_sitemap_xml config published;
      };
    ]
  in
  Ok { entries; artifacts = system_artifacts @ page_artifacts }
