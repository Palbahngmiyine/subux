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
  let base_folder = if src.collection = "contents" || src.collection = "docs" then "" else src.collection in
  let slug = Slug.of_key ~base_folder src.rel_path in
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
  let rendered = Markdown.render ~heading_base:2 ~page_title:title doc.body in
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

let content_hash content =
  let hash = String.fold_left (fun acc c ->
    Int64.mul (Int64.logxor acc (Int64.of_int (Char.code c))) 0x100000001b3L)
      0xcbf29ce484222325L content in
  Printf.sprintf "%08Lx" (Int64.logand hash 0xffffffffL)

let base_url (config : site_config) =
  if String.ends_with ~suffix:"/" config.base_url then
    String.sub config.base_url 0 (String.length config.base_url - 1)
  else config.base_url

let safe_href href =
  Option.value ~default:"#" (Safe_url.sanitize_url ~allow_contact:true ~decode_entities:false href)

let css_asset css = "/assets/subux-theme." ^ content_hash (Token_compiler.minify_css css) ^ ".css"

(* HTML raw-text elements must never contain a literal closing tag. *)
let escape_raw_text escape text =
  String.concat escape (String.split_on_char '<' text)

let skip_link = Html.el "a" ~attrs:[("href", "#main-content"); ("class", "skip-link")]
  [Html.txt "본문으로 건너뛰기"]

let common_head ~stylesheet ~(page_title : string) ~(description : string)
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
    Html.void "link" ~attrs:[("rel", "icon"); ("type", "image/svg+xml"); ("href", "/favicon.svg")] ();
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
    Html.void "meta" ~attrs:[("name", "theme-color"); ("media", "(prefers-color-scheme: light)"); ("content", "#fafafa")] ();
    Html.void "meta" ~attrs:[("name", "theme-color"); ("media", "(prefers-color-scheme: dark)"); ("content", "#171717")] ();
    Html.void "meta" ~attrs:[("property", "og:title"); ("content", page_title)] ();
    Html.void "meta" ~attrs:[("property", "og:description"); ("content", description)] ();
    Html.void "meta" ~attrs:[("property", "og:url"); ("content", safe_href canonical_url)] ();
    Html.void "meta" ~attrs:[("property", "og:type"); ("content", if canonical_path = "/" then "website" else "article")] ();
    Html.void "meta" ~attrs:[("property", "og:site_name"); ("content", config.title)] ();
    Html.void "meta" ~attrs:[("name", "twitter:card"); ("content", "summary")] ();
    Html.void "meta" ~attrs:[("name", "twitter:title"); ("content", page_title)] ();
    Html.void "meta" ~attrs:[("name", "twitter:description"); ("content", description)] ();
    Html.void "link"
      ~attrs:[ ("rel", "canonical"); ("href", safe_href canonical_url) ]
      ();
    (match stylesheet with
     | Some css when config.inline_css ->
         Html.el "style" ~attrs:[("data-sx-inline", "true")]
           [Html.raw (escape_raw_text "\\3c " css)]
     | css -> Html.void "link" ~attrs:[("rel", "stylesheet");
         ("href", match css with Some c -> css_asset c | None -> "/assets/subux-theme.css")] ());
    Html.void "link" ~attrs:[("rel", "preconnect"); ("href", "https://cdn.jsdelivr.net"); ("crossorigin", "anonymous")] ();
    Html.void "link" ~attrs:[("rel", "preload"); ("as", "style"); ("crossorigin", "anonymous");
      ("href", "https://cdn.jsdelivr.net/gh/orioncactus/pretendard@v1.3.9/dist/web/variable/pretendardvariable-dynamic-subset.min.css")] ();
    Html.void "link" ~attrs:[("rel", "stylesheet"); ("crossorigin", "anonymous"); ("media", "print"); ("onload", "try{for(const r of this.sheet.cssRules)if(r.type===5)r.style.fontDisplay='optional'}catch{}this.media='all'");
      ("href", "https://cdn.jsdelivr.net/gh/orioncactus/pretendard@v1.3.9/dist/web/variable/pretendardvariable-dynamic-subset.min.css")] ();
    Html.el "noscript" [Html.void "link" ~attrs:[("rel", "stylesheet"); ("crossorigin", "anonymous");
      ("href", "https://cdn.jsdelivr.net/gh/orioncactus/pretendard@v1.3.9/dist/web/variable/pretendardvariable-dynamic-subset.min.css")] ()];
  ] @ (match config.custom_css with
      | None -> []
      | Some _ -> [ Html.void "link"
          ~attrs:[ ("rel", "stylesheet"); ("href", "/assets/custom.css") ] () ])
    @ (match config.custom_js with
       | None -> []
       | Some _ -> [ Html.el "script"
           ~attrs:[ ("type", "module"); ("src", "/assets/custom.js") ] [] ])

let document ?(customized = false) ~lang ~head ~body () =
  let html = Html.to_string (Html.frag body) in
  let head = List.map (function
    | Html.El ("style", attrs, [Html.Raw css]) ->
        let css = if customized then Token_compiler.minify_css css
          else Token_compiler.critical_stylesheet ~html css in
        Html.el "style" ~attrs [Html.raw css]
    | node -> node) head in
  Html.to_document ~lang ~head ~body

let mermaid_runtime_script : Html.node =
  Html.el "script" ~attrs:[("type", "module")] [Html.raw {|const blocks=[...document.querySelectorAll('.article-content pre > code.language-mermaid')];
let loading=false,observer;
const load=()=>{if(loading)return;loading=true;if(observer)observer.disconnect();
 const run=()=>import('https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs').then(async({default:mermaid})=>{
  const probe=document.createElement('span');document.body.append(probe);
  const color=n=>{probe.style.color='var(--sx-'+n+')';return getComputedStyle(probe).color;};
  const themeVariables={background:color('color-bg'),primaryColor:color('active-fill'),primaryTextColor:color('on-active-fill'),lineColor:color('color-text')};probe.remove();
  mermaid.initialize({startOnLoad:false,securityLevel:'strict',theme:'base',themeVariables});
  for(const block of blocks){const pre=block.parentElement;if(!pre)continue;
   const diagram=document.createElement('div');diagram.className='mermaid';diagram.textContent=block.textContent;
   pre.replaceWith(diagram);try{await mermaid.run({nodes:[diagram]});}catch{diagram.replaceWith(pre);}}
 }).catch(()=>{});
 if('requestIdleCallback'in window)requestIdleCallback(run);else setTimeout(run,0);
};
if(blocks.length){if('IntersectionObserver'in window){observer=new IntersectionObserver(es=>{if(es.some(e=>e.isIntersecting))load();},{rootMargin:'200px'});blocks.forEach(b=>observer.observe(b));}
else load();}|}]

let prefetch_script = Html.el "script" ~attrs:[("data-sx-prefetch", "true")] [Html.raw
  {|(()=>{const s=new Set;for(const t of ['pointerenter','touchstart'])document.addEventListener(t,e=>{const a=e.target.closest?.('a[href^="/"]');if(!a||a.origin!==location.origin||a.hasAttribute('download')||s.has(a.href))return;s.add(a.href);const l=document.createElement('link');l.rel='prefetch';l.href=a.href;document.head.append(l)},{capture:true,passive:true})})();|}]

let callout_script = Html.el "script" [Html.raw {|document.addEventListener('toggle',e=>{const d=e.target;if(d.matches?.('details[data-callout]'))d.dataset.expanded=String(d.open)},{capture:true});|}]

let subux_tabs_runtime_script : Html.node = Html.el "script" [Html.raw {|(()=>{
const panels=[...document.querySelectorAll('[data-tab-panel]')],links=[...document.querySelectorAll('a.tab-link[data-tab-id]')];
const initial=panels.find(p=>!p.hidden)?.dataset.tabPanel;
function select(tab){if(!panels.some(p=>p.dataset.tabPanel===tab))return false;
 panels.forEach(p=>p.hidden=p.dataset.tabPanel!==tab);links.forEach(a=>{if(a.dataset.tabId===tab)a.setAttribute('aria-current','page');else a.removeAttribute('aria-current')});return true;}
function restore(){const q=new URLSearchParams(location.search).get('tab');const path=location.pathname.split('/').filter(Boolean)[0];select(q||links.find(a=>new URL(a.href).pathname.split('/').filter(Boolean)[0]===path)?.dataset.tabId||initial);}
document.addEventListener('click',e=>{const a=e.target.closest?.('a.tab-link[data-tab-id]');if(!a||e.defaultPrevented||e.button!==0||e.metaKey||e.ctrlKey||e.shiftKey||e.altKey||a.target||!select(a.dataset.tabId))return;e.preventDefault();const u=new URL(location.href);u.searchParams.set('tab',a.dataset.tabId);history.pushState(null,'',u);});
window.addEventListener('popstate',restore);restore();})();|}]

let jt_runtime_script : Html.node =
  Html.el "script" [ Html.raw {|(function () {
  const root = document.documentElement;
  let saved;
  try { saved = localStorage.getItem('sx-theme'); } catch {}
  if (saved === 'light' || saved === 'dark') root.dataset.theme = saved;
  const themeButton = document.getElementById('jt-theme-toggle');
  const systemTheme = matchMedia('(prefers-color-scheme: dark)');
  const isDark = () => root.dataset.theme ? root.dataset.theme === 'dark' : systemTheme.matches;
  function updateThemeButton() {
    if (themeButton) themeButton.setAttribute('aria-checked', String(isDark()));
  }
  updateThemeButton();
  systemTheme.addEventListener('change', updateThemeButton);
  if (themeButton) themeButton.addEventListener('click', () => {
    root.dataset.theme = isDark() ? 'light' : 'dark';
    try { localStorage.setItem('sx-theme', root.dataset.theme); } catch {}
    updateThemeButton();
  });
  const menu = document.getElementById('jt-main-nav');
  const menuButton = document.getElementById('jt-menu-toggle');
  if (menu && menuButton) {
    menu.dataset.ready = 'true';
    menuButton.addEventListener('click', () => {
      const open = menuButton.getAttribute('aria-expanded') !== 'true';
      menuButton.setAttribute('aria-expanded', String(open));
      menu.dataset.open = String(open);
      document.body.style.overflow = open ? 'hidden' : '';
    });
    matchMedia('(min-width: 960px)').addEventListener('change', () => {
      menu.dataset.open = 'false';
      menuButton.setAttribute('aria-expanded', 'false');
      document.body.style.overflow = '';
    });
  }
  const sidebar = document.getElementById('jt-sidebar');
  const sidebarButton = document.getElementById('jt-sidebar-toggle');
  const backdrop = document.getElementById('jt-sidebar-backdrop');
  const outline = document.getElementById('jt-outline');
  const outlineButton = document.getElementById('jt-outline-toggle');
  function closeSidebar(restoreFocus = true) {
    if (!sidebar || sidebar.dataset.open !== 'true') return;
    sidebar.dataset.open = 'false';
    sidebarButton.setAttribute('aria-expanded', 'false');
    backdrop.hidden = true;
    document.body.style.overflow = '';
    if (restoreFocus) sidebarButton.focus();
  }
  if (sidebar && sidebarButton && backdrop) {
    sidebar.dataset.ready = 'true';
    sidebarButton.addEventListener('click', () => {
      sidebar.dataset.open = 'true';
      sidebarButton.setAttribute('aria-expanded', 'true');
      backdrop.hidden = false;
      document.body.style.overflow = 'hidden';
      const link = sidebar.querySelector('a');
      if (link) link.focus();
    });
    backdrop.addEventListener('click', closeSidebar);
    matchMedia('(min-width: 960px)').addEventListener('change', () => closeSidebar(false));
  }
  if (outline && outlineButton) {
    outline.dataset.ready = 'true';
    outlineButton.addEventListener('click', () => {
      const open = outline.dataset.open !== 'true';
      outline.dataset.open = String(open);
      outlineButton.setAttribute('aria-expanded', String(open));
    });
    outline.addEventListener('click', event => {
      if (event.target.closest('a')) {
        outline.dataset.open = 'false';
        outlineButton.setAttribute('aria-expanded', 'false');
      }
    });
    document.addEventListener('click', event => {
      if (!outline.contains(event.target) && !outlineButton.contains(event.target)) {
        outline.dataset.open = 'false';
        outlineButton.setAttribute('aria-expanded', 'false');
      }
    });
  }
  document.querySelectorAll('.sx-code-copy').forEach(button => {
    let resetCopy;
    button.addEventListener('click', async () => {
      const code = button.parentElement.querySelector('pre code');
      if (!code) return;
      try {
        await navigator.clipboard.writeText(code.textContent);
        button.setAttribute('aria-label', 'Copied');
        button.dataset.copied = 'true';
        clearTimeout(resetCopy);
        resetCopy = setTimeout(() => {
          delete button.dataset.copied;
          button.setAttribute('aria-label', 'Copy code');
        }, 2000);
      }
      catch { button.setAttribute('aria-label', 'Copy failed'); }
    });
  });
  const modal = document.getElementById('jt-search-modal');
  const openButton = document.getElementById('jt-search-open');
  const input = document.getElementById('jt-search-input');
  const results = document.getElementById('jt-search-results');
  let indexData = null;
  let indexPromise = null;
  let previousFocus = null;
  function renderResults() {
    if (!input || !results) return;
    results.replaceChildren();
    const query = input.value.trim().toLowerCase();
    if (!query || !Array.isArray(indexData)) return;
    indexData.filter(item => (item.title || '').toLowerCase().includes(query) ||
      (item.text || '').toLowerCase().includes(query)).slice(0, 8).forEach(item => {
        const li = document.createElement('li');
        const link = document.createElement('a');
        link.href = item.href;
        link.className = 'jt-sidebar-link';
        link.textContent = item.title;
        li.append(link);
        results.append(li);
      });
  }
  async function openSearch() {
    if (!modal || !modal.hidden) return;
    previousFocus = document.activeElement;
    modal.hidden = false;
    if (input) input.focus();
    if (!indexPromise) indexPromise = fetch('/search-index.json').then(async response => {
      if (!response.ok) throw new Error('Search index unavailable');
      indexData = await response.json();
    }).catch(() => { indexPromise = null; indexData = null; });
    await indexPromise;
    if (!modal.hidden) renderResults();
  }
  function closeSearch() {
    if (!modal || modal.hidden) return;
    modal.hidden = true;
    if (previousFocus && previousFocus.isConnected) previousFocus.focus();
  }
  if (openButton) openButton.addEventListener('click', openSearch);
  if (input) input.addEventListener('input', renderResults);
  if (modal) modal.addEventListener('click', event => { if (event.target === modal) closeSearch(); });
  window.addEventListener('keydown', event => {
    if (modal && (event.metaKey || event.ctrlKey) && event.key.toLowerCase() === 'k') {
      event.preventDefault();
      if (modal.hidden) openSearch(); else closeSearch();
    } else if (event.key === 'Escape') {
      closeSidebar();
      if (outline && outlineButton) { outline.dataset.open = 'false'; outlineButton.setAttribute('aria-expanded', 'false'); }
      const searchWasOpen = modal && !modal.hidden;
      closeSearch();
      if (!searchWasOpen && menu && menuButton && menu.dataset.open === 'true') {
        menu.dataset.open = 'false';
        menuButton.setAttribute('aria-expanded', 'false');
        document.body.style.overflow = '';
        menuButton.focus();
      }
    } else if (event.key === 'Tab') {
      const activePanel = modal && !modal.hidden ? modal : sidebar && sidebar.dataset.open === 'true' ? sidebar : null;
      if (!activePanel) return;
      const focusable = Array.from(activePanel.querySelectorAll('input, a[href], button')).filter(el => el.getClientRects().length);
      const first = focusable[0], last = focusable[focusable.length - 1];
      if (!first) { event.preventDefault(); return; }
      if (event.shiftKey && document.activeElement === first) { event.preventDefault(); last.focus(); }
      else if (!event.shiftKey && document.activeElement === last) { event.preventDefault(); first.focus(); }
    }
  });
})();|} ]

let normalize_href_for_match (href : string) : string =
  let s = String.trim href in
  let len = String.length s in
  if len > 1 && s.[len - 1] = '/' then String.sub s 0 (len - 1)
  else if String.ends_with ~suffix:".md" s then
    String.sub s 0 (len - 3)
  else s


let render_jt_navbar ?(current_href = "/") ?(has_sidebar = false) (config : site_config) : Html.node =
  let nav_links =
    List.map
      (fun (item : nav_item) ->
        Html.el "a"
          ~attrs:([ ("href", safe_href item.link); ("class", "jt-nav-link") ]
            @ if normalize_href_for_match item.link = normalize_href_for_match current_href
              then [ ("aria-current", "page") ] else [])
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
          ("role", "switch");
          ("aria-checked", "false");
          ("aria-label", "Toggle color theme");
        ]
      [ Html.el "span" ~attrs:[ ("class", "jt-theme-check"); ("aria-hidden", "true") ]
          [ Html.el "span" ~attrs:[ ("class", "jt-theme-sun") ] [];
            Html.el "span" ~attrs:[ ("class", "jt-theme-moon") ] [] ] ]
  in
  let github_node =
    match config.github_url with
    | None -> Html.empty
    | Some gh_url ->
        Html.el "a"
          ~attrs:
            [
              ("href", safe_href gh_url);
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
    ~attrs:[ ("class", if has_sidebar then "jt-navbar with-sidebar" else "jt-navbar") ]
    [
      Html.el "div"
        ~attrs:[ ("class", "jt-navbar-inner") ]
        [
          Html.el "a"
            ~attrs:[ ("href", "/"); ("class", "jt-brand") ]
            [ Html.el "span" [ Html.txt config.title ] ];
          Html.el "button"
            ~attrs:[ ("type", "button"); ("id", "jt-menu-toggle");
                     ("class", "jt-menu-toggle"); ("aria-label", "Toggle navigation");
                     ("aria-controls", "jt-main-nav"); ("aria-expanded", "false") ]
            [ Html.el "span" ~attrs:[ ("class", "jt-menu-icon"); ("aria-hidden", "true") ]
                [ Html.el "span" ~attrs:[ ("class", "top") ] [];
                  Html.el "span" ~attrs:[ ("class", "middle") ] [];
                  Html.el "span" ~attrs:[ ("class", "bottom") ] [] ] ];
          Html.el "nav"
            ~attrs:[ ("id", "jt-main-nav"); ("class", "jt-nav-links");
                     ("data-ready", "false"); ("data-open", "false"); ("aria-label", "Main Navigation") ]
            (nav_links @ [ search_btn;
              Html.el "div" ~attrs:[ ("class", "jt-appearance") ]
                [ Html.el "span" ~attrs:[ ("class", "jt-appearance-label") ] [ Html.txt "Appearance" ]; theme_btn ];
              github_node ]);
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
                  ("aria-label", "Search query");
                  ("autocomplete", "off");
                ]
              ();
            Html.el "ul"
              ~attrs:
                [ ("id", "jt-search-results"); ("class", "jt-sidebar-list") ]
              [];
          ];
      ]



let search_button (config : site_config) =
  if config.enable_search then Html.el "button" ~attrs:[("type", "button");
    ("id", "jt-search-open"); ("class", "jt-search-btn"); ("aria-label", "Search ⌘K")]
    [Html.txt "Search ⌘K"] else Html.empty

(* 1. Blog_mode Home, Article & 404 Renderers *)
let page_title_attrs (entry : parsed_entry) =
  match entry.rendered.page_title_id with None -> [] | Some id -> [("id", id)]

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

let render_blog_home ?stylesheet ~(config : site_config) ~(active_tab : string)
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
              ("href", safe_href gh_url);
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
            ("href", "/" ^ t.id ^ "/");
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
    common_head ~stylesheet ~page_title ~description:config.description ~canonical_path:"/"
      ~config
  in
  let body =
    [
      Html.el "main"
        ~attrs:[ ("class", "site-container"); ("id", "main-content"); ("tabindex", "-1") ]
        [
          Html.el "div"
            ~attrs:[ ("class", "page-wrapper") ]
            [
              Html.el "header"
                ~attrs:[ ("class", "site-header") ]
                [ Html.el "h1" [ Html.txt config.title ]; search_button config; github_node ];
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
      render_jt_search_modal config;
      jt_runtime_script;
      subux_tabs_runtime_script;
    ]
  in
  document ~customized:(config.custom_css <> None || config.custom_js <> None)
    ~lang:config.lang ~head ~body:(skip_link :: body @ [prefetch_script; callout_script]) ()

let render_blog_article ?stylesheet ~(config : site_config)
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
    common_head ~stylesheet ~page_title:entry.title ~description:desc
      ~canonical_path:(Slug.to_href entry.slug)
      ~config
  in
  let schema = Json_parser.Object ([
    ("@context", Json_parser.String "https://schema.org");
    ("@type", Json_parser.String "BlogPosting");
    ("headline", Json_parser.String entry.title);
    ("url", Json_parser.String (safe_href (base_url config ^ Slug.to_href entry.slug))) ] @
    (match entry.meta.date with None -> [] | Some d -> [("datePublished", Json_parser.String (format_iso d))])) in
  let head = head @ [Html.el "script" ~attrs:[("type", "application/ld+json")]
    [Html.raw (escape_raw_text "\\u003c" (Json_parser.to_string schema))]] in
  let scripts =
    [render_jt_search_modal config; jt_runtime_script] @
    (if entry.rendered.has_mermaid then [ mermaid_runtime_script ] else [])
  in
  let body =
    [
      Html.el "main"
        ~attrs:[ ("class", "site-container"); ("id", "main-content"); ("tabindex", "-1") ]
        [
          Html.el "section"
            ~attrs:[ ("class", "content-wrapper") ]
            [
              Html.el "nav"
                ~attrs:[ ("class", "article-nav"); ("aria-label", "Article navigation") ]
                [
                  Html.el "a"
                    ~attrs:[ ("href", "/"); ("class", "back-link") ]
                    [ Html.txt "← 처음으로" ];
                  search_button config;
                ];
              Html.el "article"
                [
                  Html.el "header"
                    ~attrs:[ ("class", "article-header") ]
                    [ Html.el "h1" ~attrs:(page_title_attrs entry) [ Html.txt entry.title ]; date_node ];
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
  document ~customized:(config.custom_css <> None || config.custom_js <> None)
    ~lang:config.lang ~head ~body:(skip_link :: body @ [prefetch_script; callout_script]) ()

let render_not_found_page ?stylesheet ~(config : site_config) () : string =
  let page_title = "이곳엔 아무것도 없습니다 | " ^ config.title in
  let head =
    common_head ~stylesheet ~page_title ~description:config.description
      ~canonical_path:"/404.html" ~config
  in
  let body =
    [
      Html.el "main"
        ~attrs:[ ("class", "site-container"); ("id", "main-content"); ("tabindex", "-1") ]
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
  document ~customized:(config.custom_css <> None || config.custom_js <> None)
    ~lang:config.lang ~head ~body:(skip_link :: body @ [prefetch_script; callout_script]) ()

let render_not_found ~config = render_not_found_page ~config ()

(* 2. Docs_mode Navbar, Search Modal, Home & Documentation Page Renderers *)
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

let render_docs_home ?stylesheet ~(config : site_config) (entry : parsed_entry) :
    string =
  let hero_node =
    match entry.meta.hero with
    | None ->
        Html.el "section"
          ~attrs:[ ("class", "jt-hero") ]
          [
            Html.el "h1"
              ~attrs:(("class", "jt-hero-name") :: page_title_attrs entry)
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
              ~attrs:(("class", "jt-hero-name") :: page_title_attrs entry)
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
    common_head ~stylesheet ~page_title:config.title ~description:config.description
      ~canonical_path:"/" ~config
  in
  let body =
    [
      render_jt_navbar config;
      Html.el "main" ~attrs:[("id", "main-content"); ("tabindex", "-1")] [ hero_node; features_node; body_md_node ];
      render_jt_search_modal config;
      jt_runtime_script;
      (if entry.rendered.has_mermaid then mermaid_runtime_script else Html.empty);
    ]
  in
  document ~customized:(config.custom_css <> None || config.custom_js <> None)
    ~lang:config.lang ~head ~body:(skip_link :: body @ [prefetch_script; callout_script]) ()

let render_docs_page ?stylesheet ~(config : site_config)
    ~(all_docs : parsed_entry list) (entry : parsed_entry) : string =
  let current_href = Slug.to_href entry.slug in
  let norm_cur = normalize_href_for_match current_href in
  let groups = effective_sidebar_groups config all_docs in
  let sidebar_node =
    Html.el "aside"
      ~attrs:[ ("id", "jt-sidebar"); ("class", "jt-sidebar");
               ("data-ready", "false"); ("data-open", "false"); ("aria-label", "Sidebar Navigation") ]
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
                            ("href", safe_href item.link);
                            ("class", "jt-sidebar-link active");
                            ("aria-current", "page");
                          ]
                        else
                          [ ("href", safe_href item.link); ("class", "jt-sidebar-link") ]
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
        ~attrs:[ ("id", "jt-outline"); ("class", "jt-outline");
                 ("data-ready", "false"); ("data-open", "false"); ("aria-label", "On this page") ]
        []
    else
      Html.el "aside"
        ~attrs:[ ("id", "jt-outline"); ("class", "jt-outline");
                 ("data-ready", "false"); ("data-open", "false"); ("aria-label", "On this page") ]
        [
          Html.el "a" ~attrs:[ ("class", "jt-outline-top"); ("href", "#") ]
            [ Html.txt "Return to top" ];
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
          ~attrs:[ ("href", safe_href href); ("class", "jt-pager-link prev") ]
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
          ~attrs:[ ("href", safe_href href); ("class", "jt-pager-link next") ]
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
    common_head ~stylesheet ~page_title ~description:config.description
      ~canonical_path:current_href ~config
  in
  let scripts =
    if entry.rendered.has_mermaid then
      [ jt_runtime_script; mermaid_runtime_script ]
    else [ jt_runtime_script ]
  in
  let body =
    [
      render_jt_navbar ~current_href ~has_sidebar:true config;
      Html.el "div" ~attrs:[ ("class", "jt-local-nav") ]
        [ Html.el "button"
            ~attrs:[ ("type", "button"); ("id", "jt-sidebar-toggle");
                     ("aria-controls", "jt-sidebar"); ("aria-expanded", "false") ]
            [ Html.el "span" ~attrs:[ ("class", "jt-local-menu-icon"); ("aria-hidden", "true") ] [];
              Html.txt "Menu" ];
          Html.el "button"
            ~attrs:[ ("type", "button"); ("id", "jt-outline-toggle");
                     ("aria-controls", "jt-outline"); ("aria-expanded", "false") ]
            [ Html.txt "On this page";
              Html.el "span" ~attrs:[ ("class", "jt-local-chevron"); ("aria-hidden", "true") ] [] ] ];
      Html.el "button"
        ~attrs:[ ("type", "button"); ("id", "jt-sidebar-backdrop");
                 ("class", "jt-sidebar-backdrop"); ("aria-label", "Close document navigation");
                 ("hidden", "hidden") ] [];
      Html.el "div"
        ~attrs:[ ("class", "jt-doc-layout") ]
        [
          sidebar_node;
          Html.el "main"
            ~attrs:[ ("class", "jt-doc-main"); ("id", "main-content"); ("tabindex", "-1") ]
            [
              Html.el "article"
                ~attrs:[ ("class", "article-content") ]
                [ Html.el "h1" ~attrs:(page_title_attrs entry) [Html.txt entry.title]; Html.raw entry.rendered.html ];
              pager_footer;
            ];
          outline_node;
        ];
      render_jt_search_modal config;
    ]
    @ scripts
  in
  document ~customized:(config.custom_css <> None || config.custom_js <> None)
    ~lang:config.lang ~head ~body:(skip_link :: body @ [prefetch_script; callout_script]) ()

(* 3. Search Index & Sitemap Generators *)
let utf8_prefix limit text =
  let n = min (String.length text) (max 0 limit) in
  let rec boundary n =
    if n = 0 || n = String.length text || Char.code text.[n] land 0xc0 <> 0x80 then n
    else boundary (n - 1)
  in String.sub text 0 (boundary n)

let normalize_whitespace text =
  String.split_on_char ' ' (String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) text)
  |> List.filter (fun s -> s <> "") |> String.concat " "

let build_search_index_json (entries : parsed_entry list) : string =
  let entries = List.filter (fun (e : parsed_entry) -> not e.meta.draft) entries in
  let item (e : parsed_entry) text = Json_parser.Object [
    ("title", Json_parser.String e.title); ("href", Json_parser.String (Slug.to_href e.slug));
    ("collection", Json_parser.String e.collection); ("text", Json_parser.String text)] in
  let metadata = Json_parser.to_string (Json_parser.Array (List.map (fun e -> item e "") entries)) in
  let budget = max 0 ((19000 - String.length metadata) / max 1 (List.length entries)) in
  let preview e =
    let headings = String.concat " " (List.map (fun (t : toc_item) -> t.text) e.rendered.toc) in
    let text = normalize_whitespace (headings ^ " " ^ e.rendered.plain_text) in
    let rec fit n =
      let text = utf8_prefix n text in
      if String.length (Json_parser.to_string (Json_parser.String text)) - 2 <= budget || n = 0 then text
      else fit (n - 1)
    in fit (min 400 budget)
  in
  Json_parser.to_string (Json_parser.Array (List.map (fun e -> item e (preview e)) entries))

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
          render_blog_home ~stylesheet:css_bundle ~config ~active_tab:first_tab_id published
        in
        let tab_pages =
          List.map
            (fun (t : collection_tab) ->
              {
                out_path = t.id ^ "/index.html";
                content = render_blog_home ~stylesheet:css_bundle ~config ~active_tab:t.id published;
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
                content = render_blog_article ~stylesheet:css_bundle ~config ~series e;
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
                content = render_docs_home ~stylesheet:css_bundle ~config home_entry;
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
                content = render_docs_home ~stylesheet:css_bundle ~config synthetic_home;
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
                     render_docs_page ~stylesheet:css_bundle ~config ~all_docs:published e;
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
          render_blog_home ~stylesheet:css_bundle ~config ~active_tab:first_tab_id published
        in
        let tab_pages = List.map (fun (t : collection_tab) ->
          {out_path=t.id ^ "/index.html"; content=render_blog_home ~stylesheet:css_bundle ~config ~active_tab:t.id published}) tabs in
        let rendered_pages =
          List.filter_map
            (fun (e : parsed_entry) ->
              if Slug.to_string e.slug = "" && e.meta.layout = Some Layout_home
              then
                Some
                  {
                    out_path = "index.html";
                    content = render_docs_home ~stylesheet:css_bundle ~config e;
                  }
              else if Slug.to_string e.slug = "" then None
              else if e.meta.layout = Some Layout_doc || e.collection = "contents" || e.collection = "docs"
              then
                Some
                  {
                    out_path = out_path_of_slug e.slug;
                    content =
                      render_docs_page ~stylesheet:css_bundle ~config ~all_docs:published e;
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
                    content = render_blog_article ~stylesheet:css_bundle ~config ~series e;
                  })
            published
        in
        if List.exists (fun a -> a.out_path = "index.html") rendered_pages then
          tab_pages @ rendered_pages
        else { out_path = "index.html"; content = home_html } :: tab_pages @ rendered_pages
  in
  let system_artifacts =
    [
      { out_path = "favicon.svg"; content = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 64 64\"><text x=\"8\" y=\"50\" font-size=\"52\">S</text></svg>" };
      { out_path = "assets/subux-theme.css"; content = css_bundle };
      { out_path = String.sub (css_asset css_bundle) 1 (String.length (css_asset css_bundle) - 1); content = Token_compiler.minify_css css_bundle };
      { out_path = "_headers"; content =
          css_asset css_bundle ^ "\n  Cache-Control: public, max-age=31536000, immutable\n/assets/subux-theme.css\n  Cache-Control: public, max-age=0, must-revalidate\n/*\n  X-Content-Type-Options: nosniff\n  X-Frame-Options: DENY\n  Referrer-Policy: strict-origin-when-cross-origin\n" };
      { out_path = "robots.txt"; content = "User-agent: *\nAllow: /\nSitemap: " ^ base_url config ^ "/sitemap.xml\n" };
      { out_path = "404.html"; content = render_not_found_page ~stylesheet:css_bundle ~config () };
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
  let custom_artifacts = match config.custom_css with
    | None -> []
    | Some content -> [ { out_path = "assets/custom.css"; content } ]
  in
  let script_artifacts = match config.custom_js with
    | None -> []
    | Some content -> [ { out_path = "assets/custom.js"; content } ]
  in
  Ok { entries; artifacts = system_artifacts @ custom_artifacts @ script_artifacts @ page_artifacts }
