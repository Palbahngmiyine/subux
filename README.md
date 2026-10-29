# Subux

OCaml 5와 Dune으로 Markdown 워크스페이스를 정적 사이트로 빌드합니다.
`blog`, `docs`, `unified` 모드를 지원합니다.

```sh
dune build
dune exec subux -- init --mode docs site
dune exec subux -- build -i site -o _site
```

## Cloudflare adapter

Workers Static Assets용 배포 산출물을 생성합니다. OCaml 빌드에 Node.js나
추가 opam 패키지가 필요하지 않으며, 배포할 때만 Cloudflare CLI를 사용합니다.
Worker 실행 코드와 서버 측 렌더링은 추가하지 않습니다.

저장소 루트에서 실행합니다.

```sh
dune exec subux -- build --adapter cloudflare --name my-subux -i site -o _site
```

`site/subux.json`의 `baseUrl`을 실제 사이트 주소로 설정하면 canonical URL과
sitemap에도 그 주소가 반영됩니다. 기존 `--mode`, `--tone`, `--title`, `--tokens`
옵션을 함께 사용할 수 있습니다. 다른 디렉터리에서 실행할 때는 필요에 따라
`--tokens /path/to/tokens.json`을 지정합니다.

배포 도구는 **cf 1.0.0-beta.25**에서 검증했습니다. 이 버전의 Node.js 요구사항은
`^22.23.0 || ^24.22.0 || ^26.12.0`입니다. 아래 명령은 도구 설치·로그인·검사·배포를
순서대로 수행하며, 마지막 명령이 실제 사이트를 공개합니다.

```sh
npm install --global cf@1.0.0-beta.25
cf auth login
cd _site
cf deploy --prebuilt --dry-run
cf deploy --prebuilt
```

CI에서는 대화형 로그인 대신 `CLOUDFLARE_API_TOKEN`과
`CLOUDFLARE_ACCOUNT_ID` 환경 변수를 배포 단계에 설정합니다. 인증 정보는
Subux 설정이나 생성 파일에 넣지 않습니다. OCaml이 없는 배포 환경으로
`_site/.cloudflare/output/v0/` 전체를 복사한 뒤, 그 `.cloudflare` 디렉터리의
부모에서 `cf deploy --prebuilt`를 실행할 수 있습니다.

| 옵션 | 기본값 | 동작 |
| --- | --- | --- |
| `--adapter static\|cloudflare` | `static` | 일반 정적 파일 또는 Cloudflare Build Output 생성 |
| `--name` | `subux-site` | 배포할 Worker 이름. 사이트마다 다른 이름 지정 권장 |
| `--compatibility-date` | `2026-10-29` | Cloudflare 호환성 날짜 (`YYYY-MM-DD`) |
| `-o`, `--output` | `_site` | 생성 산출물의 루트 디렉터리 |

```text
_site/.cloudflare/output/v0/
├── .subux-generated
├── config.json
└── workers/default/
    ├── worker.config.json
    └── assets/
        ├── index.html
        ├── 404.html
        ├── assets/subux-theme.css
        └── ...
```

정적 파일만 `workers/default/assets/`에 포함합니다. 문서 URL은 기존처럼
확장자와 끝 슬래시 없이 제공하도록 설정하며, 존재하지 않는 경로에는 생성된
`404.html`을 HTTP 404로 제공하도록 설정합니다. SPA fallback은 사용하지 않습니다.

재빌드는 `.subux-generated` 표시가 있는 `v0/` 전체를 교체하므로 삭제하거나
draft로 전환한 글이 이전 산출물에 남지 않습니다. 이 디렉터리를 직접 수정하지
마세요. 표시가 없는 기존 `v0/`와 심볼릭 링크로 된 출력 경로는 오류로 거부합니다.
같은 출력 루트의 그 밖의 파일은 보존합니다.

`check --adapter cloudflare`는 파일을 쓰지 않고 사이트와 adapter 설정을
검사합니다. `--adapter`를 생략하면 기존 정적 빌드 형식을 유지합니다.

Cloudflare Build Output v0와 cf는 아직 beta 규격입니다. CLI를 갱신할 때는
dry run으로 호환성을 다시 확인하세요. Subux는 배포·도메인 연결·인증을 자동으로
실행하지 않습니다.

공식 자료:
[사전 빌드 배포](https://developers.cloudflare.com/cf/projects/#deploy-a-prebuilt-build),
[HTML URL 처리](https://developers.cloudflare.com/workers/static-assets/routing/advanced/html-handling/),
[Build Output 구현](https://github.com/cloudflare/workers-sdk/tree/main/packages/build-output-utils).

## 검증

```sh
dune build
dune runtest
dune exec subux -- check
```

회귀 테스트는 세 모드의 정적 파일 일치, adapter 설정과 경로 검증,
삭제·draft 처리, 실패한 빌드에서의 이전 출력 보존, 출력 디렉터리 보호를 검사합니다.
