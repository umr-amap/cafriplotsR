# Deploying the Query Plots app on SSP Cloud

This folder publishes the **Query Plots** app (`launch_query_plots_app()`) as a
hosted web app on [SSP Cloud](https://datalab.sspcloud.fr), so anyone can use it
from a browser without installing R or RStudio.

It mirrors `../taxonomic_match/` exactly; read that folder's README for the
shared background. Only the differences are spelled out here.

## How it fits together

| File | Role |
|------|------|
| `../../inst/app/query_plots/app.R` | Entry point sourced by shiny-server; returns the app object |
| `Dockerfile` | Builds an image (`rocker/shiny` + CafriplotsR) |
| `../../.github/workflows/docker-query-plots.yml` | CI: build & push the image to ghcr.io |
| `Chart.yaml` | Helm chart inheriting SSP Cloud's generic `shiny` chart |
| `values.yaml` | Image, public hostname, resources |
| `templates/probe-patch.yaml` | Post-install hook loosening the chart's tight health probe |

## What differs from taxonomic-match

| | taxonomic-match | query-plots |
|---|---|---|
| Image | `ghcr.io/umr-amap/cafri-taxomatch` | `ghcr.io/umr-amap/cafri-queryplots` |
| Release | `cafri-taxomatch` | `cafri-queryplots` |
| Hostname | `cafri-taxomatch.lab.sspcloud.fr` | `cafri-queryplots.lab.sspcloud.fr` |
| Offline mode | offered (cached backbone) | not offered - the app needs the live database |
| Extra system package | — | `zip` (for `utils::zip()` CSV / shapefile downloads) |

Both releases can live in the same namespace and **share one
`cafri-public-credential` Secret**; create it once, not once per app.

## One-time setup

1. Push this branch to `master`, or run Actions tab → "Build query-plots Docker
   image" → Run workflow.
2. Repo **Packages** tab → open `cafri-queryplots` → Package settings → Change
   visibility → **Public**. Without this SSP Cloud cannot pull the image.
3. Create the public credential Secret if it does not already exist in the
   namespace (shared with taxonomic-match):

   ```bash
   kubectl get secret cafri-public-credential || \
   kubectl create secret generic cafri-public-credential \
     --from-literal=CAFRI_PUBLIC_USER=CafriP_public \
     --from-literal=CAFRI_PUBLIC_PASS='<current password>'
   ```

4. Confirm `shiny.image.repository` and `ingress.hostname` in `values.yaml`.

## Deploy

From a **VSCode/RStudio service on SSP Cloud launched with the Kubernetes Role
set to `admin`** (the default `view` role cannot patch deployments):

```bash
cd deployment/query_plots
helm dependency update      # fetch the generic shiny sub-chart into charts/
helm install cafri-queryplots . -f values.yaml

# Verify
helm ls
kubectl get pods
kubectl logs -l app.kubernetes.io/instance=cafri-queryplots
```

The app goes live at `https://cafri-queryplots.lab.sspcloud.fr`.

## Redeploy after a code change

```bash
# 1. Rebuild (from your machine):
git push origin master        # -> Actions rebuilds ghcr.io/.../cafri-queryplots:latest
#    Wait for the workflow to go green before step 2.

# 2. Roll the pod (from the admin-role SSP Cloud terminal):
kubectl rollout restart deployment cafri-queryplots-shiny
kubectl rollout status  deployment cafri-queryplots-shiny
```

`rollout restart` re-pulls because the tag is `latest` with
`pullPolicy: Always`. Only a values/chart change needs
`helm upgrade cafri-queryplots . -f values.yaml`.

A stale UI after all this is usually browser caching - hard-refresh with
Ctrl-Shift-R.

## Local sanity check (Docker only, no Kubernetes)

```bash
docker build -f deployment/query_plots/Dockerfile -t cafri-queryplots:test .
docker run --rm -p 3838:3838 cafri-queryplots:test
# open http://localhost:3838
```

Worth doing before pushing: it catches a missing system library or a broken
entry point in ~15 minutes instead of after a CI round trip.

## Before publishing - read this

- **Egress to OVH**: the cluster must allow outbound connections to
  `dg474899-001.dbaas.ovh.net:35699`. Already proven by the taxonomic-match
  deployment, so no re-test needed if that app works.
- **Public URL**: `*.lab.sspcloud.fr` is reachable by anyone, so the public
  read-only login becomes world-usable. For query-plots that exposes the whole
  plot/individual inventory to anonymous download (Excel, CSV, shapefile) at
  whatever the `CafriP_public` role is granted in PostgreSQL. **Check that
  role's row-level-security grants before going live** - this app hands out
  bulk data, where the taxonomic matcher hands out names.
- **Concurrency caveat**: connection pools live in the package-global
  `.db_env`, so two *simultaneous* users connecting with **different**
  credentials can clobber each other's pool. Harmless when everyone uses the
  same public login; a real limit for concurrent authenticated users.
- **Memory**: plot queries materialise large result sets and render leaflet
  maps and plotly figures in the same process. The 8Gi limit in `values.yaml`
  is a starting point - watch `kubectl top pod` after the first real use.
