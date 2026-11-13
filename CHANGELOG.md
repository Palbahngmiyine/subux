# Changelog

## [0.3.0](https://github.com/Palbahngmiyine/subux/compare/v0.2.0...v0.3.0) (2026-11-13)


### Features

* themeColor·fontDisplay·lazyImages·trailingSlash 설정 추가 ([fab125c](https://github.com/Palbahngmiyine/subux/commit/fab125c26694b06720a661d95ac7e47c4f1bf3ff))
* 릴리스에 npm 형식 패키지(subux-&lt;version&gt;.tgz) 추가 ([91bd3fc](https://github.com/Palbahngmiyine/subux/commit/91bd3fc88edd5ed7b68b8e879ab93f8c6b55aa28))
* 사이트 설정 4종과 npm 형식 릴리스 패키지 추가, 코드 강조 보완 ([5fbec86](https://github.com/Palbahngmiyine/subux/commit/5fbec86faf35dbda1b51e09d0fbc4902a1c7f05f))
* 실행 파일 옆의 design/tokens.json을 기본 토큰으로 사용 ([ef58c48](https://github.com/Palbahngmiyine/subux/commit/ef58c48096bd30cc480f425d331e9b6dd55bdb8d))


### Bug Fixes

* JSON 코드 강조가 구두점을 highlight.js처럼 토큰으로 감싸게 함 ([e8d2440](https://github.com/Palbahngmiyine/subux/commit/e8d244097f366396ecd9813308545cfc81878a75))
* 리뷰에서 나온 토크나이저·토큰 탐색·게시 검증 문제를 보완 ([8f2902b](https://github.com/Palbahngmiyine/subux/commit/8f2902b241dcde008107e4b960794bfbd04a62e5))
* 코드 강조가 셸 URL을 주석으로 칠하지 않게 하고 YAML·JSON 토큰을 보완 ([b240f58](https://github.com/Palbahngmiyine/subux/commit/b240f58a43dc9c85bc90ecd7898bbebfba31be4a))

## [0.2.0](https://github.com/Palbahngmiyine/subux/compare/v0.1.0...v0.2.0) (2026-11-10)


### Features

* headLinks로 아이콘과 manifest 링크 지정 ([faaa754](https://github.com/Palbahngmiyine/subux/commit/faaa754b80362a94ebfed862ab83c04b44df9d42))
* public 디렉터리 파일을 결과물에 그대로 복사 ([2137b2b](https://github.com/Palbahngmiyine/subux/commit/2137b2bc9dba4dfa9fc3c531171c853bb0f198a9))
* public 정적 파일 복사와 headLinks 지원 ([4ab0d62](https://github.com/Palbahngmiyine/subux/commit/4ab0d622cd7b54c7d71ebfc8c013e41ae90bcbbc))


### Bug Fixes

* Markdown 탐색이 끊어진 링크·순환 링크·FIFO에서 멈추지 않게 함 ([72973d9](https://github.com/Palbahngmiyine/subux/commit/72973d9978697a55619849adb059d9c43446ae54))
* public 경로 충돌을 대소문자 무시로 검사 ([f186298](https://github.com/Palbahngmiyine/subux/commit/f186298f9ad798bd53cde8bda97456b742a46f0d))
* public 복사에서 점 파일을 건너뜀 ([e704520](https://github.com/Palbahngmiyine/subux/commit/e7045207f47adf98c1f0f745951b501092229967))

## 0.1.0 (2026-11-04)


### ⚠ BREAKING CHANGES

* **ssg:** site_config 직접 생성 시 custom_css·custom_js 필드가, rendered_markdown 직접 생성 시 page_title_id 필드가 필요합니다.
* **ssg:** rendered_markdown 레코드를 직접 구성하는 호출자는 page_title_id 필드를 추가해야 합니다.
* **ssg:** site_config 레코드를 직접 구성하는 호출자는 custom_css와 custom_js 필드를 추가해야 합니다. 기본 설정에는 두 필드가 None으로 제공됩니다.
* **ssg:** site_config를 직접 생성하는 호출자는 inline_css 필드를 추가해야 한다

### Features

* **adapter:** Cloudflare Workers 정적 사이트 배포 지원 ([ee38d91](https://github.com/Palbahngmiyine/subux/commit/ee38d91d9833918f1e065de18695e9ede662cff2))
* **adapter:** Cloudflare 정적 사이트 배포 adapter 추가 ([a8ec650](https://github.com/Palbahngmiyine/subux/commit/a8ec650577e1459b09510b9cc256ba7170e1022c))
* **ssg:** 사이트별 프로필과 문서 렌더링 호환성 추가 ([781b80a](https://github.com/Palbahngmiyine/subux/commit/781b80a75a042edde075246b022b749a3308f3df))


### Bug Fixes

* **ci:** 릴리스 복구 검증 테스트와 조회 실패 처리 추가 ([4354a7c](https://github.com/Palbahngmiyine/subux/commit/4354a7cb33e904cd38353e655bd75256395f296b))
* **ssg:** Codex 리뷰 수정과 예제·검증 문서 제거 ([e897661](https://github.com/Palbahngmiyine/subux/commit/e8976617f35fe5264c4166eb3bca6f2416d835a1))
* **ssg:** Mermaid 색상과 Markdown 제목 레벨 회귀 수정 ([2de8b75](https://github.com/Palbahngmiyine/subux/commit/2de8b75474e18e6fae1dc95b13b89f2b126f24ad))
* **ssg:** 네비게이션·모바일 메뉴·중첩 프론트매터 리뷰 반영 및 autoSidebar 설정 지원 ([ded212c](https://github.com/Palbahngmiyine/subux/commit/ded212c492e8dd6835937a6686ea794869ee0cd0))
* **ssg:** 문서 기능 복원과 목차 리뷰 수정 ([9dc649e](https://github.com/Palbahngmiyine/subux/commit/9dc649e77d86837a10308e8785f1356108869efb))
* **ssg:** 최신 테마 최적화와 사이트 프로필 통합 ([c27b4ef](https://github.com/Palbahngmiyine/subux/commit/c27b4efdd598a8bb33409c015bffe76ab23191ea))


### Performance Improvements

* **ssg:** Lighthouse 최적화와 페이지별 임계 CSS 적용 ([5052ce6](https://github.com/Palbahngmiyine/subux/commit/5052ce6140f9c5c1eea7766384ea6593df8df512))


### Documentation

* **ssg:** 커스텀 자산 설정의 라이브러리 호환성 명시 ([f65c420](https://github.com/Palbahngmiyine/subux/commit/f65c420feea7632fbf07516ab34c65e54747da9e))
