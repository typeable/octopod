#!/bin/bash
set -e
message () {
    echo "==> $1"
}

echo
echo "    🔖  If you encounter any issues during the installation please open an issue:"
echo "          https://github.com/typeable/octopod/issues/new"
echo
echo "    ℹ️  Please note that you might experience performance issues if you have low resouce limits:"
echo "          https://github.com/typeable/octopod/blob/master/docs/en/Octopod_deployment_guide.md#if-you-want-to-try-it-locally"
echo

for cmd in kind kubectl helm; do
   if ! command -v $cmd &> /dev/null; then
           message "$cmd is not installed. Please, install it!"
           exit 1;
   fi
done
if [ "$(helm version --short | awk -F '.' '{print $1}')" != "v3" ]; then
    message 'Your version of helm is not supported. Please install helm 3'
    exit 1
fi
if [ "$(kind version | awk -F ' ' '{print $2}')" != "v0.11.1" ]; then
    message "Warning! This kind version hasn't been tested"
fi
if kind get clusters | grep -q octopod; then
    echo -n 'Octopod kind cluster already exists. Do you want do delete it? [y/n] ';
    read confirm
    if [ "$confirm" != 'y' ]; then
       message "Exiting"
       exit 0
    else
       message "Deleting and recreating cluster"
       kind delete cluster --name octopod
    fi
fi
message "Creating kind cluster"
cat <<EOF | kind create cluster --config=-
kind: Cluster
apiVersion: kind.x-k8s.io/v1alpha4
name: octopod
networking:
  kubeProxyMode: "ipvs"
nodes:
- role: control-plane
  kubeadmConfigPatches:
  - |
    kind: InitConfiguration
    nodeRegistration:
      kubeletExtraArgs:
        node-labels: "ingress-ready=true"
  extraPortMappings:
  - containerPort: 80
    hostPort: 80
    protocol: TCP
  - containerPort: 443
    hostPort: 443
    protocol: TCP
EOF
cat <<EOF | kubectl --context=kind-octopod apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: coredns
  namespace: kube-system
data:
  Corefile: |
    .:53 {
        errors
        health {
           lameduck 5s
        }
        ready
        kubernetes cluster.local in-addr.arpa ip6.arpa {
           pods insecure
           fallthrough in-addr.arpa ip6.arpa
           ttl 30
        }
        prometheus :9153
        forward . 8.8.8.8 1.1.1.1 {
           max_concurrent 1000
        }
        cache 30
        loop
        reload
        loadbalance
    }
EOF
message "Installing ingress-nginx controller"
kubectl --context=kind-octopod apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/controller-v0.45.0/deploy/static/provider/kind/deploy.yaml
has_error=0
while true; do
        set +e
        kube_out=`kubectl --context=kind-octopod -n ingress-nginx get deploy 2>&1`
        err_code=$?
        set -e
        if [ $err_code != 0 ] && [ $has_error == 0 ]; then
                echo "kubectl returned: $kube_out"
                echo "Do you want to continue installation? [y/n] "
                read confirm
                if [ "$confirm" != 'y' ]; then
                        message "Deleting cluster..."
                        kind delete cluster --name octopod
                        exit 0;
                else
                        has_error=1
                fi
        fi
        if [ $err_code == 0 ] && [ $( echo -n "$kube_out" | awk -F ' ' '$1 ~ /^ingress-nginx/ {print $4}') == '1' ]; then
                message 'ingress-controller is ready'
                break;
        else
                message 'waiting for ingress-controller to become ready...'; sleep 5;
        fi
done
message "Creating nessesary namespaces"
kubectl --context=kind-octopod create ns octopod
kubectl --context=kind-octopod create ns octopod-deployment
message "Adding typeable helm repository"
helm repo add typeable https://typeable.github.io/octopod
helm repo update
message "Installing octopod helm chart"
helm --kube-context=kind-octopod -n octopod install octopod typeable/octopod --wait --timeout=5m --set octopod.baseDomain="lvh.me" --set ingress.tls.enabled=false
echo
echo "    🌟  Don't forget to star Octopod on GitHub:"
echo "          https://github.com/typeable/octopod"
echo
echo "    🗑️  To uninstall Octopod just run:"
echo "          kind delete cluster --name octopod"
