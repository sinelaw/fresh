// The hosts a workspace may be on (types/fresh.d.js HostSpec).

// A host as people read it: an SSH target, a container, the agent.
/** function hostText(host: HostSpec) => String */
export function hostText(host) {
  if (host.kind === "ssh") return host.target;
  if (host.kind === "container") return `container ${host.id}`;
  return host.kind;
}
/** function localHost() => HostSpec */
export function localHost() { return { kind: "local" }; }
/** function sshHost(target: String, identity: String | Null, options: String[]) => HostSpec */
export function sshHost(target, identity, options) { return { kind: "ssh", target, identity, options }; }
