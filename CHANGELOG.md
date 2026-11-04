# Changelog

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
