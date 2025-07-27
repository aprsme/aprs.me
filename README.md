# Aprs

## Prerequisites

Before setting up the project, ensure you have the following installed:
- Elixir 1.17+
- Erlang/OTP
- PostgreSQL with PostGIS extension

## Setup

To start your Phoenix server:

  * Run the complete setup:
    ```bash
    mix setup
    ```
  * Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Learn more

  * Official website: https://www.phoenixframework.org/
  * Guides: https://hexdocs.pm/phoenix/overview.html
  * Docs: https://hexdocs.pm/phoenix
  * Forum: https://elixirforum.com/c/phoenix-forum
  * Source: https://github.com/phoenixframework/phoenix

## Kubernetes (K3s) Deployment

This application can be automatically deployed to a K3s Kubernetes cluster using GitHub Actions.

### Prerequisites

1.  **K3s Cluster:** You need a running K3s cluster.
2.  **`kubectl` access:** Ensure you have `kubectl` configured to interact with your K3s cluster.
3.  **Container Registry:** This setup uses GitHub Container Registry (GHCR) to store the application's Docker image.

### Initial Setup

#### 1. GitHub Actions Secrets

The GitHub Actions workflow requires the following secret to be configured in your repository's settings (Settings -> Secrets and variables -> Actions -> Repository secrets):

*   `KUBE_CONFIG_DATA`: The base64 encoded content of your K3s kubeconfig file. This file grants GitHub Actions permission to deploy to your cluster.
    *   To get this value, you can typically run:
        ```bash
        cat ~/.kube/config | base64 -w 0
        ```
        (If you're on macOS, you might need `cat ~/.kube/config | base64`).
        Or, directly from the K3s server node:
        ```bash
        sudo cat /etc/rancher/k3s/k3s.yaml | base64 -w 0
        ```
    *   Ensure the user in this kubeconfig has sufficient permissions in your K3s cluster to create namespaces, deployments, services, PVCs, and secrets in the target namespace (`aprs-app`).

#### 2. Kubernetes Secrets for the Application

The application requires secrets for database connection, Phoenix secret key base, etc. These are managed by a Kubernetes Secret object named `aprs-secrets` in the `aprs-app` namespace.

1.  **Copy the example secrets file:**
    ```bash
    cp k8s/secrets.yaml.example k8s/secrets.yaml
    ```
2.  **Edit `k8s/secrets.yaml`:**
    Open `k8s/secrets.yaml` and replace the placeholder values with your actual credentials and configuration:
    *   `POSTGRES_USER`: Your desired PostgreSQL username.
    *   `POSTGRES_PASSWORD`: Your desired PostgreSQL password.
    *   `SECRET_KEY_BASE`: A strong secret key for Phoenix. You can generate one using `mix phx.gen.secret`.
    *   `APRS_CALLSIGN`, `APRS_PASSCODE`: Your APRS credentials, if applicable.
    *   `PHX_HOST`: The domain name or IP where your application will be accessible.

3.  **Apply the secrets to your K3s cluster:**
    Make sure you are targeting your K3s cluster with `kubectl`.
    ```bash
    kubectl apply -f k8s/namespace.yaml # Ensure namespace exists
    kubectl apply -f k8s/secrets.yaml -n aprs-app
    ```
    **Important:** `k8s/secrets.yaml` is included in `.gitignore` and should **not** be committed to the repository with your actual secrets.

### GitHub Actions CI/CD Workflow

The workflow is defined in `.github/workflows/deploy-k3s.yaml`. It will:
1.  Trigger on pushes to the `main` branch.
2.  Build the application's Docker image using the provided `Dockerfile`.
3.  Push the image to GitHub Container Registry (GHCR), tagged with the Git SHA and `latest`. The image will be named `ghcr.io/<YOUR_GITHUB_USERNAME_OR_ORG>/<YOUR_REPO_NAME>`.
4.  Deploy the application and its PostgreSQL database to the K3s cluster using the manifests in the `k8s/` directory.
    *   It automatically updates `k8s/app-deployment.yaml` to use the newly built image tag.

### Application Configuration Notes

*   **Database Migrations:** The Kubernetes deployment for the application includes an init container that automatically runs database migrations (`/app/bin/migrate`) before the main application container starts.
*   **Health Check Endpoint:** The application deployment (`k8s/app-deployment.yaml`) is configured with readiness and liveness probes that expect a health check endpoint at `/health` (returning HTTP 200). You may need to add this to your Phoenix application's router and controller:
    *   In `lib/aprsme_web/router.ex`:
        ```elixir
        scope "/", AprsmeWeb do
          pipe_through :browser
          # ... other routes
          get "/health", PageController, :health
        end
        ```
    *   In `lib/aprsme_web/controllers/page_controller.ex` (or another suitable controller):
        ```elixir
        def health(conn, _params) do
          # You can add more sophisticated checks here if needed
          json(conn, %{status: "ok", version: Application.spec(:aprsme, :vsn)})
        end
        ```
*   **Image Name:** The application deployment manifest (`k8s/app-deployment.yaml`) uses a placeholder image name `your-ghcr-username/aprs-app:latest`. The CI/CD pipeline automatically replaces this with the correct image name from GHCR (e.g., `ghcr.io/your-github-owner/your-repo-name:<git-sha>`).

### Accessing the Application

The application service (`k8s/app-service.yaml`) is configured with `type: NodePort`.
1.  Find the NodePort assigned by Kubernetes:
    ```bash
    kubectl get svc aprs-app-service -n aprs-app -o=jsonpath='{.spec.ports[0].nodePort}'
    ```
2.  Access your application at `http://<your-k3s-node-ip>:<nodePort>`.

If your K3s cluster is configured with an external load balancer and Ingress, you might want to change the service type to `ClusterIP` and create an Ingress resource for more sophisticated routing and SSL termination.
