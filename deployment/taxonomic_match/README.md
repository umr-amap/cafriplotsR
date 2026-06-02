# Deploying the Taxonomic Match app on SSP Cloud

This folder contains everything needed to publish the **Taxonomic Name
Standardization** app (`launch_taxonomic_match_app()`) as a hosted web app on
[SSP Cloud](https://datalab.sspcloud.fr), so anyone can use it from a browser
without installing R or RStudio.

## How it fits together

| File | Role |
|------|------|
| `../../inst/app/taxonomic_match/app.R` | Entry point sourced by shiny-server; returns the app object |
| `Dockerfile` | Builds an image (`rocker/shiny` + CafriplotsR) |
| `../../.github/workflows/docker-taxonomic-match.yml` | CI: build & push the image to GitHub Container Registry (ghcr.io) |
| `Chart.yaml` | Helm chart inheriting SSP Cloud's generic `shiny` chart |
| `values.yaml` | Image, public hostname, resources, env vars |

The database stays **external** (OVH, `dg474899-001.dbaas.ovh.net:35699`).
Users authenticate inside the app through the login module — with their own
credentials, the **public read-only user**, or the **offline cached
backbone** — so no database secret is stored in the image or the chart.

## One-time setup

No external registry account or secrets are required — CI pushes to ghcr.io
using GitHub's built-in `GITHUB_TOKEN`.

1. Run the build workflow once (push to `master`, or Actions tab → "Build
   taxonomic-match Docker image" → Run workflow).
2. In the repo's **Packages** tab, open the new `cafri-taxomatch` package and
   set its visibility to **Public** (Package settings → Change visibility).
   This lets SSP Cloud pull it anonymously, for free, with no pull rate limits.
3. Confirm `shiny.image.repository` in `values.yaml` matches your repo owner:
   `ghcr.io/umr-amap/cafri-taxomatch` (must be lowercase).
4. Pick a hostname in `values.yaml` (`*.lab.sspcloud.fr`).

## Deploy

```bash
# 1. Image is built/pushed automatically by GitHub Actions on push to master,
#    or trigger it manually (Actions tab -> "Build taxonomic-match Docker image").

# 2. From a VSCode/RStudio service on SSP Cloud (with namespace admin rights):
cd deployment/taxonomic_match
helm dependency update      # fetch the generic shiny sub-chart
helm install cafri-taxomatch . -f values.yaml

# 3. Verify
helm ls
kubectl get pods
kubectl logs <pod-name>     # watch app startup

# 4. Update after a new image is pushed
helm upgrade cafri-taxomatch . -f values.yaml
```

The app will be live at the `ingress.hostname` you set.

## Redeploy after a code change

Whenever you change the package code (R functions, `DESCRIPTION`, the Dockerfile
or the entry point), the running pod keeps the *old* image until you rebuild and
roll it. Two steps:

```bash
# 1. Rebuild the image (from your local machine, after committing):
git push origin master
#    -> GitHub Actions rebuilds and pushes ghcr.io/umr-amap/cafri-taxomatch:latest
#       Wait for the workflow to go green (Actions tab) before step 2.

# 2. Roll the pod to the new image (from the SSP Cloud terminal):
kubectl rollout restart deployment cafri-taxomatch-shiny
kubectl get pods -w        # Ctrl-C once the new pod is 1/1 Running
```

`rollout restart` works because the image tag is `latest` with
`pullPolicy: Always`, so the new pod re-pulls the freshly built image. If you
ever pin a specific tag (e.g. a commit SHA) in `values.yaml` instead, update the
tag there and run `helm upgrade cafri-taxomatch . -f values.yaml` instead.

Only a values/chart change (not a code change) needs `helm upgrade`; a pure code
change just needs the rebuild + `rollout restart` above.

## Health probe (handled automatically)

The upstream `shiny` sub-chart hardcodes an HTTP `GET /` liveness/readiness
probe. Because open-source shiny-server uses a **single shared R process**, that
probe is rendered by the same worker that loads the taxonomic backbone — so
while a backbone load is in progress the worker is blocked, the probe times out,
and Kubernetes restarts the pod (a crash loop, seen when two sessions run in
parallel).

The sub-chart does not expose the probe via values, so `templates/probe-patch.yaml`
runs a **post-install / post-upgrade Helm hook** that patches the deployment to a
**TCP-socket probe on 3838** (it checks the listener is up, independent of the
busy worker). This re-applies on every `helm install` and `helm upgrade`, so an
upgrade can never reintroduce the bad probe — no manual `kubectl patch` needed.

To activate it on an existing release, run `helm upgrade cafri-taxomatch . -f values.yaml`
once. If the hook image (`probePatch.image`, default `bitnami/kubectl:latest`)
can't be pulled on your cluster, override it in `values.yaml`.

Note: this stops the crash, but Shiny is still single-process — a long backbone
load by one user will briefly freeze others. Acceptable for low concurrency; for
heavy concurrent use, run multiple replicas with sticky sessions or ShinyProxy.

## Before publishing — read this

- **Egress to OVH**: the SSP Cloud cluster must allow outbound connections to
  `dg474899-001.dbaas.ovh.net:35699`. Test it once from an SSP Cloud service
  (`pg_isready -h ... -p 35699` or a quick `DBI::dbConnect`) before deploying.
- **Public URL**: `*.lab.sspcloud.fr` is reachable by anyone. The embedded
  public read-only user becomes effectively world-usable. That is acceptable
  for read-only taxonomy/traits, but make it a conscious choice.
- **Concurrency caveat**: the apps currently store connection pools in a
  shared package-global (`.db_env`), and query helpers like `call.mydb.taxa()`
  read from it. Two *simultaneous* users connecting with **different**
  credentials can clobber each other's pool. For a public deployment where
  everyone uses the **same** public (or offline) connection this is harmless.
  Supporting many concurrent *authenticated, differently-permissioned* users
  would require threading per-session pools through the modules — a larger
  refactor, out of scope for this scaffold.

## Local sanity check (Docker only, no Kubernetes)

```bash
docker build -f deployment/taxonomic_match/Dockerfile -t cafri-taxomatch:test .
docker run --rm -p 3838:3838 cafri-taxomatch:test
# open http://localhost:3838
```

## Notes on the free workflow

- **GitHub Actions** build minutes are free on this public repo.
- **ghcr.io** stores public images for free with no pull rate limits — unlike
  DockerHub, whose anonymous pull limits cause intermittent `ImagePullBackOff`
  on shared clusters like SSP Cloud.
- **SSP Cloud** is free for the research/education community.

So end to end this deployment incurs no cost. The only manual gate is making
the ghcr.io package Public (step 2 above).
