# Lighthouse 최적화 검증 기록

2026-10-28. 기준 커밋: `3d72f11da1b66ed3f53d0a49ed1cf4253c25cb97`.

`LIGHTHOUSE-TASKS.md`의 13개 구현 항목과 후속 CSS 축소 요청을 반영했다. 백로그는 프로젝트 규칙에 따라 Git 제외 상태로 작업 트리에 보관한다. 아래 점수는 합성 콘텐츠를 생성해 gzip을 제공하는 로컬 서버에서 측정한 결과다. 운영 배포나 임의의 사용자 콘텐츠에 대한 점수 보장은 아니다.

## 실행 환경과 결과

- OCaml 5.5.1, Dune 3.24.2, Node.js 24.21.0.
- Lighthouse 12.8.2, Headless Chrome 156.0.0.0, macOS.
- Lighthouse 모바일 기본 설정 및 기본 시뮬레이션 throttling 사용. 성능 점수 산출 방식과 감사 항목을 변경하지 않았다.
- 실제 jsDelivr 폰트 CSS와 폰트 요청을 사용했다. Lighthouse 실행에는 요청 차단이나 네트워크 응답 대체를 적용하지 않았다.
- 로컬 서버에서 gzip과 생성한 `_headers`에 해당하는 보안 헤더를 제공했다. 이 서버 구성은 SSG가 자동으로 배포하거나 압축을 제공한다는 의미가 아니다.

| 페이지 | Performance | Accessibility | Best Practices | SEO | FCP/LCP (ms) | TBT (ms) | CLS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Blog 홈 | 100 | 100 | 100 | 100 | 639 | 0 | 0.000 |
| Blog 본문 | 100 | 100 | 100 | 100 | 650 | 0 | 0.000 |
| Blog 정적 탭 | 100 | 100 | 100 | 100 | 640 | 0 | 0.000 |
| Docs 홈 | 100 | 100 | 100 | 100 | 644 | 0 | 0.000 |
| Docs 본문 | 100 | 100 | 100 | 100 | 1389 | 0 | 0.000 |
| Unified 홈 | 100 | 100 | 100 | 100 | 638 | 0 | 0.000 |
| Unified 문서 | 100 | 100 | 100 | 100 | 658 | 0 | 0.000 |
| `404.html` 파일 | 100 | 100 | 100 | 100 | 629 | 0 | 0.000 |

`404.html`은 정적 파일을 HTTP 200으로 읽는 검사다. 존재하지 않는 URL의 HTTP 상태 코드와 호스팅 제공자의 오류 페이지 라우팅은 검사하지 않았다.

## CSS 감소량과 보존 범위

| 산출물 | 크기 (bytes) | 원본 전체 CSS 대비 감소 |
| --- | ---: | ---: |
| 원본 전체 CSS | 32,127 | — |
| 공백·주석 압축 / 해시 CSS 파일 | 27,567 | 14.2% |
| 전체 UI 인라인 최적화 | 21,232 | 33.9% |
| Blog/Unified 홈 임계 CSS | 7,081 | 78.0% |
| Docs 홈 임계 CSS | 6,592 | 79.5% |
| Blog 본문 임계 CSS | 8,357 | 74.0% |
| Docs/Unified 본문 임계 CSS | 10,164 | 68.4% |

홈 CSS의 gzip 크기는 Blog 2,051 bytes, Docs 1,948 bytes다. CSS를 일괄 7KB 이하로 만들었다고 주장하지 않는다. 콘텐츠와 활성 컴포넌트에 따라 필요한 규칙이 달라진다.

TASK-102의 단순 공백 스캐너만으로는 30% 감소가 불가능했다. 사용자의 추가 지시인 “극한으로 줄일 수 있을 때 까지 줄여”에 따라 인라인 최적화를 확장했다. `minify_css`의 측정값과 확장된 파이프라인의 측정값을 구분한다.

- `minify_css`: 문자열, escape, URL 내부 바이트, 후손 선택자와 `calc()` 공백을 보존하는 순수 스캐너다.
- `inline_stylesheet`: 중복 테마 선언을 `light-dark()`로 통합하고, 사용되는 변수의 의존성을 따라 필요한 선언을 남기며, 반복 참조에 짧은 내부 별칭을 부여한다.
- `critical_stylesheet`: 페이지에 존재하는 요소·클래스·속성에 필요한 규칙만 선택한다. 검색 결과와 Mermaid처럼 런타임에 생성되는 클래스는 유지한다. 미디어 쿼리와 상태 선택자도 유지한다.
- 외부 CSS는 모든 공개 `--sx-*` 토큰과 기존 미디어 쿼리 테마 구조를 유지한다. 해시 파일은 이 전체 CSS를 공백 압축한 내용의 FNV-1a 해시로 명명한다.
- 기본 인라인 CSS는 CSS Color 5 `light-dark()`를 지원하는 브라우저를 대상으로 한다. 구형 브라우저가 필요하면 `subux.json`의 `"inlineCss": false`로 전체 외부 CSS를 사용한다.

규격: [CSS Color 5의 light-dark()](https://drafts.csswg.org/css-color-5/#light-dark), [CSS Color Adjustment의 color-scheme](https://www.w3.org/TR/css-color-adjust-1/#color-scheme-prop).

## 항목별 구현

| 항목 | 구현과 검증 |
| --- | --- |
| TASK-101 | CSS `@import` 제거. 모든 head에 preconnect, CSS preload, print/onload 로딩과 noscript 폴백 제공. |
| TASK-102 | 기본 인라인 CSS와 `inlineCss` 설정 추가. 위 표의 단계별 크기 검증 및 CSS 동등성 비교. |
| TASK-103 | FNV-1a 검사 벡터, 내용 기반 CSS 경로와 `_headers` 검증. 변경 가능한 무해시 CSS 별칭에는 재검증 캐시를 사용하고 immutable은 실제 해시 경로에만 적용. |
| TASK-201 | IntersectionObserver 200px 임계값 뒤 idle 시점에 Mermaid import. observer 즉시 해제, import 실패 시 원문 보존. 첫 화면 요청 0회, 스크롤 후 1회 확인. |
| TASK-202 | 450 bytes 미만 위임 prefetch, 동기 탭 전환과 URL/뒤로 가기 복원. JavaScript 없이 정적 탭 경로 이동 확인. |
| TASK-203 | 네이티브 details/summary 사용. Enter, Space, 포인터 및 JavaScript 비활성 상태 조작 확인. `toggle` 위임은 호환용 data 속성만 동기화. |
| TASK-204 | 공백 정규화, UTF-8 경계 보존, heading 우선 검색 요약과 항목별 적응형 바이트 예산. 120개 긴 한글/이모지 문서의 인덱스가 20KB 미만이고 항목 누락이 없는지 회귀 검사. 블로그/문서 공통 검색을 열 때만 인덱스 요청. |
| TASK-301 | 이미지 lazy/async, 양수 `800x450` 제목 구문, 이미지 크기·비디오 비율 CSS. 기존 안전하지 않은 URL 차단 테스트 유지. |
| TASK-302 | 실제 `.jt-*` 클래스 전체를 테마 CSS와 일치시킴. 반응형 열, 터치 크기, 숨겨진 모달, 문서/Unified 탐색 확인. |
| TASK-303 | 보정된 로컬 Gothic 폴백. CORS 폰트 CSS onload에서 font-display를 optional로 설정한 뒤 적용해 늦은 폰트 교체 방지. 1.2초 폰트 지연 테스트와 CLS 재측정. |
| TASK-401 | 코드 주석 `#57606a`, 콜아웃 제목의 본문 토큰 사용, 본문 링크 밑줄. 라이트/다크 axe 위반 0건 확인. |
| TASK-402 | 모든 템플릿의 첫 body 요소인 skip link, main 대상, 정확히 하나의 h1. 본문·인용·리스트·콜아웃·각주 heading 정규화와 템플릿 ID 예약. |
| TASK-403 | 두 theme-color, OpenGraph/Twitter, BlogPosting JSON-LD와 robots.txt. JSON-LD의 `</script>` 주입 회귀 검사. |

폰트 메트릭은 [Pretendard v1.3.9의 실제 WOFF2](https://cdn.jsdelivr.net/gh/orioncactus/pretendard@v1.3.9/packages/pretendard/dist/web/variable/woff2-dynamic-subset/PretendardVariable.subset.0.woff2)에서 읽었다. UPM 2048, hhea ascent 1950, descent -494, lineGap 0에 따라 ascent 95.21484375%, descent 24.12109375%를 사용한다. 로컬 Apple SD Gothic Neo의 UPM은 1000, ascent 900, descent -300이다. 전각 한글에 대해 size-adjust 100%를 사용한다. Windows의 Malgun Gothic 실행은 검증하지 않았다.

## 브라우저 검증과 한계

- 8개 페이지 × 3개 폭(390/1024/1440px) × 라이트/다크의 48개 조건에서 인라인/전체 CSS의 계산 스타일 차이 0건.
- 이 CSS 비교는 외부 폰트 CSS를 빈 응답으로 고정해 폰트 다운로드 시점이라는 변수를 제거했다. 사용자 정의 속성 자체와 `color-scheme`은 비교에서 제외했다. 원래의 모든 토큰은 외부 CSS에 보존되며, 인라인에서는 미사용 토큰을 제거하기 때문이다.
- 실제 외부 폰트를 켜고 스타일시트를 수동 교체한 별도 실험에서는 일부 글자의 기하 속성 차이가 관찰됐다. 이 원본은 `browser-check-fonts-uncontrolled.txt`에 보존했다. 이를 삭제하거나 CSS 비교 통과 결과로 합산하지 않았다.
- 실제 폰트를 사용하는 Lighthouse는 8개 모두 CLS 0이다. 별도의 모바일 390px 폰트 요청 지연(1.2초) 테스트도 Blog 본문·Docs 홈·Docs 본문에서 CLS 0, 가로 넘침 없음이었다.
- 라이트/다크 각각 Blog 홈/본문, Docs 홈/본문의 axe-core 4.10.2 전체 검사: 위반 0건.
- 검색 최초 요청 0회, 검색 열기 후 1회, Escape 뒤 포커스 복귀 확인. 프리페치 중복 없음과 탭 전환 시 navigation entry 1개 유지, 뒤로 가기 복원 확인.
- JavaScript 비활성 상태의 `/tech/` 탭과 콜아웃 열기 확인. 정적 서버는 쿼리별 HTML을 만들지 않으므로 `?tab=` 상태 복원은 JavaScript가 담당한다. JavaScript 없는 탭 이동은 `/articles/`, `/tech/`, `/translations/`를 사용한다.
- 인덱스의 미리보기 예산은 제목·경로·컬렉션 메타데이터를 보존한다. 메타데이터 자체가 20KB를 넘는 임의의 대규모 사이트까지 총 크기를 20KB로 제한하지 않는다.
- 임의 콘텐츠의 이미지 크기는 자동 추측하지 않는다. 크기를 아는 이미지는 `![설명](url "800x450")`로 지정한다. 치수 없는 콘텐츠까지 CLS 0을 보장하지 않는다.

## 실행 명령과 산출물

필수 검증은 모두 통과했다.

```sh
dune build
dune runtest
dune exec subux -- check
git diff --check
```

`_build/` 약 16MB, 네이티브 실행 파일 약 3.3MB. OCaml/opam 런타임 의존성·PPX·워크플로우를 추가하지 않았다. 새 라이브러리 로직에 I/O나 가변 상태를 추가하지 않았다.

검증 도구는 저장소 밖 `/private/tmp/subux-audit-tools`에만 설치했다. 합성 입력, 실행한 브라우저/감사 스크립트와 최종 Lighthouse JSON은 Git 제외 경로 `output/lighthouse/`에 보관한다. `output/lighthouse/summary.json`은 최종 8개 측정값, `browser-summary.json`은 기능·axe·CLS 검사 결과다. `responsive-check.txt`에는 24개 페이지/폭 조합 각각의 라이트/다크 CSS 비교가 있다. 원본 JSON의 소수점 값은 위 표에서 반올림했다.

재실행은 `output/lighthouse/fixtures/*-src`를 `dune exec subux -- build`로 생성하고, 보관된 `subux-serve.py`/`subux-serve-gzip.py`로 제공한 뒤 `subux-lighthouse-audit.py`를 실행한다. 스크립트의 도구 경로는 위 임시 설치 위치를 사용한다. 호스팅 배포·push·PR 생성은 수행하지 않았다.
