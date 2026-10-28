# Subux Color Theme

웹·앱·TUI 전반에 일관된 시각적 인상을 유지하기 위한 디자인 토큰 명세다. 파일은 두 개뿐이다.

- `tokens.json`: 토큰 전체. 값은 모두 최종값(hex, px, ms)이고 라이트·다크가 한 토큰 안에 나란히 있다.
- `COLOR-THEME.md`(이 문서): 토큰을 읽는 법, 구현 절차, 규칙, 토큰 표, 명암비, 플랫폼별 매핑.

별도의 외부 빌드 도구나 프레임워크는 필요하지 않다. 구현자(사람 또는 LLM)가 0절의 규칙대로 `tokens.json`을 CSS 변수, 네이티브 색 자산, 터미널 스타일로 변환한다.

표의 **상태** 열은 `기준`(원본 팔레트에서 직접 정의한 기본값)과 `파생`(기본값의 명암비·보색 규칙으로부터 유도한 값)을 구분한다. JSON에서는 토큰의 `$extensions["dev.subux"].status`, 또는 묶음 전체가 같은 성격을 가질 때는 그룹의 `$description`이 같은 정보를 담는다.

## 0. tokens.json 읽는 법과 구현 절차

### 0.1 형식

W3C Design Tokens Community Group(DTCG) 형식이다. 토큰은 `$value`·`$type`·`$description`·`$extensions`를 갖고, `$type`은 그룹에서 상속된다. 이 테마가 더한 규칙은 하나다.

- `$value`가 단일 값이면 라이트·다크 **공통**이다. 예: `"primary": { "$value": "#c2e7ff" }`
- `$value`가 `{ "light": ..., "dark": ... }`면 모드별로 **다르다**. 예: `"bg": { "$value": { "light": "#fafafa", "dark": "#171717" } }`

참조(`{a.b}`)는 쓰지 않는다. 다른 토큰과 같은 값이면 값을 그대로 적고 `$description`에 `= color.text`처럼 밝힌다. `palette` 그룹은 시맨틱 토큰의 출처를 보여 주는 원시 팔레트이며 UI에서 직접 쓰지 않고 CSS로 내보내지도 않는다.

### 0.2 타입별 CSS 값

| `$type` | JSON 값 | CSS 값 |
| --- | --- | --- |
| `color` | `"#c2e7ff"`, 알파는 8자리 `"#00000014"` | 그대로 |
| `dimension`, `duration` | `"12px"`, `"150ms"` | 그대로 |
| `fontFamily` | 이름 배열 | `", "`로 잇고 공백이 있는 이름만 따옴표: `"Pretendard Variable", Pretendard, -apple-system, …` |
| `shadow` | `{offsetX, offsetY, blur, spread, color}` | `offsetX offsetY blur color` (spread는 모두 0이라 생략): `0 4px 12px #00000014` |
| `cubicBezier` | `[0.4, 0, 0.2, 1]` | `cubic-bezier(0.4, 0, 0.2, 1)` |

### 0.3 CSS 변수 이름

토큰 경로의 `.`을 `-`로 바꾸고 접두사 `sx`를 붙인다. `palette.*`는 제외한다.

| 토큰 경로 | CSS 변수 |
| --- | --- |
| `color.primary` | `--sx-color-primary` |
| `color.status.success.fg` | `--sx-color-status-success-fg` |
| `color.code.keyword` | `--sx-color-code-keyword` |
| `shadow.md` | `--sx-shadow-md` |
| `font.sans` | `--sx-font-sans` |
| `radius.md`, `space.2`, `duration.fast`, `easing.standard`, `blur.md`, `size.touch-target-min` | `--sx-radius-md`, `--sx-space-2`, `--sx-duration-fast`, `--sx-easing-standard`, `--sx-blur-md`, `--sx-size-touch-target-min` |

접두사는 Tailwind·Bootstrap 등의 변수와 겹치지 않게 하기 위한 것이다. 바꿔도 되지만 한 프로젝트 안에서는 하나만 쓴다.

### 0.4 CSS 파일 구조

라이트가 기본이고, 시스템 다크 설정 또는 `<html data-theme="dark">`에서 다크 값으로 바뀐다. `data-theme="light"`는 시스템 설정을 무시하고 라이트를 고정한다. 다크 블록에는 `{light, dark}` 형태인 토큰만 들어간다. 두 다크 블록의 내용은 같다.

```css
:root {
  color-scheme: light dark;
  --sx-color-bg: #fafafa;
  --sx-color-text: #1f1f1f;
  --sx-color-primary: #c2e7ff;          /* 공통 토큰: 라이트 블록에만 */
  --sx-color-on-primary: #1f1f1f;
  --sx-color-status-success-fg: #116329;
  --sx-shadow-md: 0 4px 12px #00000014;
  --sx-font-sans: "Pretendard Variable", Pretendard, -apple-system, BlinkMacSystemFont, system-ui, Roboto, "Helvetica Neue", "Segoe UI", "Apple SD Gothic Neo", "Noto Sans KR", "Malgun Gothic", sans-serif;
  --sx-radius-md: 12px;
  --sx-duration-fast: 150ms;
  --sx-easing-standard: cubic-bezier(0.4, 0, 0.2, 1);
  /* … tokens.json 의 나머지 토큰 (palette 제외) … */
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --sx-color-bg: #171717;
    --sx-color-text: #ececec;
    --sx-color-status-success-fg: #3fb950;
    --sx-shadow-md: 0 4px 12px #00000080;
    /* … {light, dark} 토큰의 dark 값 전부 … */
  }
}
:root[data-theme="dark"] {
  /* 위 다크 블록과 같은 내용 */
}
```

### 0.5 LLM 구현 절차

1. `tokens.json`을 읽는다. 값은 그대로 쓰면 되고 계산이나 참조 해석은 없다.
2. 0.2~0.4 규칙으로 변수 파일(예: `theme.css`)을 만든다. `palette`를 제외한 모든 토큰을 빠짐없이 넣는다.
3. 컴포넌트 스타일은 변수만 참조한다. 리터럴 색을 쓰지 않는다.
4. 필요한 색이 없으면 가장 가까운 토큰을 쓰고, `tokens.json`에 토큰 추가를 제안한다. 새 값을 만들지 않는다.
5. 터미널·네이티브는 8절의 변환 규칙을 따른다.

## 1. 정체성 한 문단

거의 무채색인 바탕(`#fafafa`) 위에 짙은 회색 글자(`#1f1f1f`)를 쓰고, 강조색은 파스텔 하늘색 `#c2e7ff` 하나만 쓴다. 강조색은 글자색이 아니라 **칠하는 색**이다. 링크·버튼·활성 탭·선택 행은 파스텔 배경 위에 어두운 글자를 올린 알약(pill) 모양으로 표현한다. 두 번째 영역을 구분해야 할 때만 하늘색의 정확한 보색인 파스텔 주황 `#ffdac2`를 같은 방식으로 쓴다. 모서리는 8~20px로 둥글고, 그림자는 검정 2~12% 알파로 얕다. 글꼴은 Pretendard 계열 산세리프다.

## 2. 규칙 (LLM이 지켜야 할 것)

1. **색은 보조 신호다.** 색으로 구분한 것은 텍스트나 형태로도 구분한다. 활성 탭은 `[이름]` 또는 밑줄, 키는 `<Key>`, 오류는 `✗`, 경고는 `!`, 성공은 `✓`처럼 아이콘이나 글자를 함께 쓴다.
2. **강조색은 항상 배경과 글자색을 쌍으로 지정한다.** `primary` 위에는 `on-primary`, `accent` 위에는 `on-accent`만 올린다. 파스텔 배경 위 글자색은 라이트·다크 모드 모두 `#1f1f1f`다. 덕분에 알약은 어느 모드에서든 같은 모습으로 읽힌다.
3. **파스텔 강조색을 글자색으로 쓰지 않는다.** `#c2e7ff`를 `#fafafa` 위 글자로 쓰면 명암비 1.24:1로 읽히지 않는다. 글자만으로 링크를 표현해야 하면 `link` 토큰(`#1f6f9a`)을 쓴다.
4. **강조색은 한 화면에 하나다.** 기본은 `primary`. 두 워크스페이스·두 모드처럼 "지금 어디에 있는가"를 화면 전체가 알려야 할 때만 `accent`로 전환한다. 둘을 같은 화면에 섞지 않는다.
5. **상태색은 `success`·`warning`·`danger`·`info` 네 가지다.** 각각 `fg`(글자·아이콘·테두리)와 `soft`(배경)만 있다. 상태색을 장식으로 쓰지 않는다.
6. **표면 단계는 세 개다.** `bg`(페이지) → `surface-muted`(코드 블록·표 머리·콜아웃·인용 배경) → `bg-elevated`(떠 있는 패널, 반투명 + `blur.md`). 테두리는 `border` 하나다.
7. **선택 영역은 반전이다.** 라이트에서는 `#171717` 배경에 `#ddd` 글자, 다크에서는 그 반대다.
8. **새 색을 만들지 않는다.** `palette.*`도 직접 쓰지 않는다. 필요한 색이 없으면 가장 가까운 시맨틱 토큰을 쓰고, `tokens.json`에 토큰 추가를 제안한다.

## 3. 시맨틱 토큰

아래 표의 토큰 이름은 `color.` 아래 경로다. 상태색은 `color.status.success.fg`처럼 한 단계 더 들어간다. CSS에서는 `var(--sx-color-primary-hover)`, `var(--sx-color-status-success-soft)`처럼 쓴다.

### 3.1 바탕·글자·테두리

| 토큰 | 라이트 | 다크 | 역할 | 상태 |
| --- | --- | --- | --- | --- |
| `bg` | `#fafafa` | `#171717` | 페이지 배경 | 라이트 기준 / 다크 파생(라이트의 선택 배경색 재사용) |
| `bg-elevated` | `#ffffffb8` (흰색 72%) | `#1f1f1fb8` | 고정 헤더·떠 있는 패널. `backdrop-filter: blur(var(--sx-blur-md))`와 함께 | 라이트 기준 / 다크 파생 |
| `surface-muted` | `#0000000a` (검정 4%) | `#ffffff0f` (흰색 6%) | 코드 블록, 표 머리, 콜아웃, 시리즈 상자, hover 배경 | 라이트 기준 / 다크 파생 |
| `border` | `#00000014` (검정 8%) | `#ffffff1f` (흰색 12%) | 모든 선. 1px 실선. 인용·콜아웃 왼쪽은 3px | 라이트 기준 / 다크 파생 |
| `text` | `#1f1f1f` | `#ececec` | 본문·제목 | 라이트 기준 / 다크 파생 |
| `text-muted` | `#666666` | `#a3a3a3` | 날짜, 라벨, 인용, 비활성 탭, 취소선 글자 | 라이트 기준 / 다크 파생 |
| `selection-bg` | `#171717` | `#dddddd` | `::selection` 배경 | 라이트 기준 / 다크 파생 |
| `selection-text` | `#dddddd` | `#171717` | `::selection` 글자 | 라이트 기준 / 다크 파생 |

### 3.2 강조색 (칠하는 색)

| 토큰 | 라이트 | 다크 | 역할 | 상태 |
| --- | --- | --- | --- | --- |
| `primary` | `#c2e7ff` | `#c2e7ff` | 링크 알약, 기본 버튼, 활성 탭 밑줄, 선택 행, 키캡 | 기준 |
| `primary-hover` | `#a3d9ff` | `#a3d9ff` | 위 요소의 hover | 기준 |
| `on-primary` | `#1f1f1f` | `#1f1f1f` | `primary` 위 글자 | 기준 |
| `accent` | `#ffdac2` | `#ffdac2` | 두 번째 영역의 알약·테두리 | 기준(`#c2e7ff`의 보색) |
| `accent-hover` | `#ffc9a3` | `#ffc9a3` | 위 요소의 hover (`primary-hover`의 보색) | 파생 |
| `on-accent` | `#1f1f1f` | `#1f1f1f` | `accent` 위 글자 | 기준 |
| `chip-neutral` | `#e4e4e4` | `#3a3a3a` | 영역에 속하지 않는 알약(상위 경로 crumb 등) | 라이트 기준 / 다크 파생 |
| `on-chip-neutral` | `#1f1f1f` | `#ececec` | `chip-neutral` 위 글자 | 파생 |
| `link` | `#1f6f9a` | `#7fcfff` | 배경 없이 글자만으로 링크를 표현해야 할 때 | 파생(라이트 AA 5.3:1 충족) |
| `focus-ring` | `#2b8cbf` | `#7fcfff` | `:focus-visible` 2px 외곽선, offset 2px | 라이트 기준 / 다크 파생 |
| `brand-mark` | `#33434c` | `#c2e7ff` | 단색 브랜드 아이콘(`currentColor`) | 기준 |

### 3.3 상태색

모두 파생이다. 라이트 값은 GitHub Primer 라이트 팔레트 계열에서 골랐고, `success.soft`·`danger.soft`는 diff 추가·삭제 배경과 같다.

| 토큰 | 라이트 fg | 라이트 soft | 다크 fg | 다크 soft | 아이콘 |
| --- | --- | --- | --- | --- | --- |
| `status.success` | `#116329` | `#dafbe1` | `#3fb950` | `#3fb95026` (fg 15%) | `✓` |
| `status.warning` | `#9a6700` | `#fff8c5` | `#d29922` | `#d2992226` | `!` |
| `status.danger` | `#cf222e` | `#ffebe9` | `#ff7b72` | `#ff7b7226` | `✗` |
| `status.info` | `#0550ae` | `#ddf4ff` | `#79c0ff` | `#79c0ff26` | `i` |

`soft` 위에는 본문 글자색(`text`)이나 같은 상태의 `fg`를 올린다. 터미널에서는 ANSI 16색의 Green·Yellow·Red를 그대로 쓰고 터미널 팔레트에 맡긴다.

행(row) 상태는 `color.row.*`에 값을 반복해 적어 두었다. `normal`은 `text`, `pending`은 `status.warning.fg`, `failed`은 `status.danger.fg`, `done`은 `text-muted`와 같다.

## 4. 명암비 (WCAG 2.x, 계산값)

| 조합 | 비율 | 판정 |
| --- | --- | --- |
| `text` on `bg` (라이트) | 15.8:1 | AAA |
| `text-muted` on `bg` (라이트) | 5.5:1 | AA |
| `on-primary` on `primary` | 12.7:1 | AAA |
| `on-primary` on `primary-hover` | 10.9:1 | AAA |
| `on-accent` on `accent` | 12.6:1 | AAA |
| `on-accent` on `accent-hover` | 11.1:1 | AAA |
| `link` on `bg` (라이트) | 5.3:1 | AA |
| `focus-ring` on `bg` (라이트) | 3.6:1 | 비텍스트 UI 3:1 통과 |
| `brand-mark` on `bg` (라이트) | 9.8:1 | AAA |
| `text` on `bg` (다크) | 15.2:1 | AAA |
| `text-muted` on `bg` (다크) | 7.1:1 | AAA |
| `link` on `bg` (다크) | 10.5:1 | AAA |
| `primary` on `bg` (다크, 칠로 쓸 때) | 13.8:1 | AAA |
| 상태 fg on `bg` (라이트 4종) | 4.6~7.3:1 | AA 이상 |
| 상태 fg on `bg` (다크 4종) | 7.1~9.2:1 | AAA |
| 상태 fg on 같은 상태 soft (라이트·다크 8종) | 4.5~6.9:1 | AA 이상 |
| `primary` on `bg` (라이트, 글자로 쓸 때) | 1.2:1 | **불가** (규칙 3) |
| 코드 주석 `#57606a` on 코드 배경 (라이트) | 5.6:1 | AA |

## 5. 코드 하이라이트 (`color.code.*`)

라이트는 GitHub 라이트 계열 highlight.js 색상이고, 다크는 highlight.js `github-dark` 테마 값이다. 코드 배경은 `surface-muted`, 테두리는 `border`.

| 토큰 | 역할 | 라이트 | 다크 |
| --- | --- | --- | --- |
| `text` | 기본 글자 | `#24292f` | `#c9d1d9` |
| `comment` | 주석 | `#57606a` | `#8b949e` |
| `keyword` | 키워드 | `#cf222e` | `#ff7b72` |
| `number` | 숫자·변수·속성 | `#0550ae` | `#79c0ff` |
| `string` | 문자열·정규식 | `#0a7f5a` | `#a5d6ff` |
| `title` | 함수·섹션 제목 | `#8250df` | `#d2a8ff` |
| `type` | 타입 | `#953800` | `#ffa657` |
| `tag` | 태그·이름 | `#116329` | `#7ee787` |
| `builtin` | 내장·불릿 | `#b35900` | `#ffa657` |
| `deletion-fg` / `deletion-bg` | diff 삭제 | `#82071e` / `#ffebe9` | `#ffdcd7` / `#67060c` |
| `addition-fg` / `addition-bg` | diff 추가 | `#116329` / `#dafbe1` | `#aff5b4` / `#033a16` |
| `diagram-bg` | mermaid 다이어그램 배경 | `#f8fafc` | `#ffffff0f` (= `surface-muted`) |

## 6. 콜아웃 분류색 (`color.callout.*`, 문서 전용)

Obsidian식 콜아웃 16종을 3px 왼쪽 테두리에 다음 색으로 구분한다(Tailwind 기본 500 단계). 두 모드 공통이다. 본문 글자나 넓은 면에는 쓰지 않는다. `#eab308`·`#22c55e`처럼 라이트 배경에서 AA에 못 미치는 값이 있으므로 제목은 `color.text`로 굵게 표시하고, 종류 이름을 글자로 함께 쓴다. 분류색은 왼쪽 테두리에만 쓴다.

| 종류 | 색 | 종류 | 색 |
| --- | --- | --- | --- |
| note | `#3b82f6` | info | `#6366f1` |
| tip | `#22c55e` | todo | `#f97316` |
| important | `#a855f7` | hint | `#14b8a6` |
| warning | `#eab308` | success | `#10b981` |
| caution | `#ef4444` | question | `#8b5cf6` |
| tldr | `#06b6d4` | missing | `#f43f5e` |
| error | `#dc2626` | bug | `#ec4899` |
| example | `#78716c` | cite | `#6b7280` |

앱 UI의 상태 표시는 이 표가 아니라 3.3절의 상태색을 쓴다.

## 7. 색 이외의 토큰 (같이 써야 인상이 유지됨)

| 토큰 | 값 | 설명 |
| --- | --- | --- |
| `font.sans` | `"Pretendard Variable", Pretendard, "Subux Gothic Fallback", -apple-system, BlinkMacSystemFont, system-ui, Roboto, "Helvetica Neue", "Segoe UI", "Apple SD Gothic Neo", "Noto Sans KR", "Malgun Gothic", sans-serif` | 기본 본문 산세리프 글꼴 |
| `font.mono` | `ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace` | 코드·키캡 고정폭 글꼴 |
| `radius.sm/md/lg/xl` | 8px, 12px, 16px, 20px. 알약 링크·인라인 코드는 sm, 버튼·카드·코드 블록은 md | 모서리 반경 |
| `shadow.sm/md/lg` | 라이트 `0 1px 2px #0000000a`, `0 4px 12px #00000014`, `0 8px 24px #0000001f`. 다크는 같은 형태에 `#00000066`, `#00000080`, `#00000099` (검정 40%·50%·60%) | 그림자 단계 |
| `space.1`~`space.10` | 8px 단위(8, 16, 24, 32, 40, 48, 56, 64, 72, 80) | 간격 스케일 |
| `duration.fast/normal/slow` | 150ms, 250ms, 350ms | 전환 시간 |
| `easing.standard` | `cubic-bezier(0.4, 0, 0.2, 1)`. 유일한 곡선 | 전환 가속도 곡선 |
| `blur.sm/md/lg` | 8px, 16px, 24px. `bg-elevated`에는 md | 배경 블러 반경 |
| `size.touch-target-min` | 44px | 최소 터치 타겟 크기 |
| 제목 굵기 | h1·h2는 400, h3는 600. 크기로만 위계를 만든다(토큰 아님) | 타이포그래피 위계 |

## 8. 플랫폼 매핑

### 웹 (순수 CSS)

0절 규칙으로 변수 파일을 만들고 컴포넌트는 변수만 참조한다.

```css
.button {
  background: var(--sx-color-primary);
  color: var(--sx-color-on-primary);
  border-radius: var(--sx-radius-md);
  transition: background-color var(--sx-duration-fast) var(--sx-easing-standard);
}
.button:hover { background: var(--sx-color-primary-hover); }
.button:focus-visible { outline: 2px solid var(--sx-color-focus-ring); outline-offset: 2px; }

a:where(:not([class])) {              /* 본문 링크 = 알약 */
  background: var(--sx-color-primary);
  color: var(--sx-color-on-primary);
  border-radius: var(--sx-radius-sm);
  padding: 0 .3em;
  text-decoration: none;
}
::selection { background: var(--sx-color-selection-bg); color: var(--sx-color-selection-text); }
```

### Tailwind CSS v4 (쓸 때만)

변수 파일을 먼저 만들고, Tailwind 네임스페이스에 연결한다. 접두사 덕분에 자기 참조가 생기지 않는다. 기본 팔레트를 지우면 토큰 클래스(`bg-primary`, `text-on-primary`, `bg-success-soft`)만 남는다.

```css
@import "tailwindcss";
@import "./theme.css";
@theme inline {
  --color-*: initial;
  --color-primary: var(--sx-color-primary);
  --color-on-primary: var(--sx-color-on-primary);
  --color-success-soft: var(--sx-color-status-success-soft);
  /* 나머지 color.* 토큰도 같은 식으로. font/radius/shadow 는 --font-sans: var(--sx-font-sans) 처럼 */
}
```

### 터미널 TUI

JSON에서는 각 토큰의 `$extensions["dev.subux"].terminal`에 들어 있다. 트루컬러를 가정하지 않는다. 알약은 배경과 글자색을 모두 지정해 밝은·어두운 터미널에서 같은 모습이 되게 한다.

| 토큰 | 터미널 값 |
| --- | --- |
| `primary` | 256색 153 (`#afd7ff`, `#c2e7ff`에 가장 가까운 색) |
| `accent` | 256색 223 (`#ffd7af`, 153의 정확한 보색) |
| `on-primary`·`on-accent`·`on-chip-neutral` | 256색 235 (`#262626`) |
| `chip-neutral` | 256색 254 (`#e4e4e4`) |
| `text-muted`, `row.done` | ANSI DarkGray |
| `text`, `bg` | 터미널 기본색(`Reset`), 값은 굵게 |
| `status.success/warning/danger.fg` | ANSI Green / Yellow / Red(굵게) |

### iOS / Android / 기타

토큰 경로의 `.`을 `_`나 camelCase로 바꿔 이름을 만들고(`color_primary`, `colorStatusSuccessFg`), `{light, dark}` 토큰은 두 appearance에 각각, 단일 값 토큰은 양쪽에 같은 값을 넣는다. 8자리 hex의 마지막 두 자리가 알파다.

## 9. 흔한 요소의 조립 예

| 요소 | 조립 |
| --- | --- |
| 본문 링크 | 배경 `primary`, 글자 `on-primary`, 모서리 `radius.sm`, 좌우 padding 4~6px. hover 시 `primary-hover`. 밑줄 없음 |
| 기본 버튼 | 배경 `primary`, 글자 `on-primary`, 모서리 `radius.md`. hover `primary-hover`, 포커스 `focus-ring` |
| 보조 버튼 | 배경 투명, 글자 `text-muted`, hover 시 배경 `surface-muted`·글자 `text` |
| 탭 | 글자 `text-muted`, 활성은 글자 `text` + 아래 2px `primary` 선. 탭 줄 아래 1px `border` |
| 선택된 행 | 배경 `primary`, 글자 `on-primary`. 두 번째 영역에서는 `accent`/`on-accent` |
| 키캡·단축키 | `<Key>` 글자를 `primary` 배경 알약으로, 굵게 |
| 코드 블록 | 배경 `surface-muted`, 1px `border`, 모서리 `radius.md`, 5절 하이라이트 |
| 인용·콜아웃 | 왼쪽 3px `border`(콜아웃은 6절 색), 배경 `surface-muted`, 글자 `text-muted`(인용) |
| 표 | 머리 배경 `surface-muted`, 머리 아래·칸 사이 1px `border`, 바깥 테두리 1px + `radius.md` |
| 알림(toast) | 배경 상태 `soft`, 왼쪽 3px 상태 `fg`, 아이콘 글자 + 상태 `fg`, 본문 `text` |
| 떠 있는 헤더 | 배경 `bg-elevated` + `backdrop-filter: blur(blur.md)`, 아래 1px `border` |
| 빈 상태 | 가운데 정렬, 글자 `text-muted` |

## 10. LLM에게 넘기는 방법

새 프로젝트에 두 파일을 `design/`으로 복사하고, 그 프로젝트의 `CLAUDE.md`나 `AGENTS.md`에 다음을 넣는다.

```markdown
## 색상
UI 색은 `design/COLOR-THEME.md`의 토큰만 쓴다. 값은 `design/tokens.json`에 있고, CSS 변수는
그 문서 0절 규칙(`--sx-` + 경로)으로 만든다. 새 색이 필요하면 만들지 말고 가장 가까운 토큰을
쓰고 추가를 제안한다. 색으로 구분한 것은 글자나 형태로도 구분한다. 파스텔 강조색(`primary`,
`accent`)은 글자색으로 쓰지 않는다.
```
