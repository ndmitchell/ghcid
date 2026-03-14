#!/usr/bin/env python3
from __future__ import annotations

import os
import shlex
import shutil
import socket
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path


def env(name: str, default: str | None = None) -> str:
    value = os.environ.get(name, default)
    if value is None:
        print(f"Set {name}", file=sys.stderr)
        raise SystemExit(1)
    return value


def run(*args: str, capture: bool = False, check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(list(args), check=check, text=True, capture_output=capture)


def gcloud(*args: str, capture: bool = False, check: bool = True) -> subprocess.CompletedProcess[str]:
    return run("gcloud", *args, capture=capture, check=check)


def log(message: str) -> None:
    print(f"\033[34m[ssh-windows-gcp]\033[0m {message}", flush=True)


@dataclass
class Context:
    project: str
    zone: str
    instance: str
    machine_type: str
    image_project: str
    image_family: str
    boot_disk_size: str
    boot_disk_type: str
    user: str
    password: str
    port: int
    firewall_rule: str
    idle_timeout_seconds: int
    baked_image_name: str | None
    startup_script: Path


class Resource:
    name: str
    dependencies: tuple["Resource", ...] = ()

    def ensure(self, ctx: Context) -> None:
        for dependency in self.dependencies:
            dependency.ensure(ctx)
        if self.exists(ctx):
            log(f"{self.name} already exists")
            return
        log(f"Creating {self.name}")
        self.create(ctx)

    def exists(self, ctx: Context) -> bool:
        raise NotImplementedError

    def create(self, ctx: Context) -> None:
        raise NotImplementedError


class FirewallRuleResource(Resource):
    name = "firewall rule"

    def exists(self, ctx: Context) -> bool:
        result = gcloud(
            "compute",
            "firewall-rules",
            "describe",
            ctx.firewall_rule,
            f"--project={ctx.project}",
            check=False,
        )
        return result.returncode == 0

    def create(self, ctx: Context) -> None:
        gcloud(
            "compute",
            "firewall-rules",
            "create",
            ctx.firewall_rule,
            f"--project={ctx.project}",
            f"--allow=tcp:{ctx.port}",
            "--target-tags=ssh",
        )


class ImageResource(Resource):
    name = "baked image"

    def exists(self, ctx: Context) -> bool:
        if not ctx.baked_image_name:
            return False
        result = gcloud(
            "compute",
            "images",
            "describe",
            ctx.baked_image_name,
            f"--project={ctx.project}",
            check=False,
        )
        return result.returncode == 0

    def create(self, ctx: Context) -> None:
        if not ctx.baked_image_name:
            raise SystemExit("BAKED_IMAGE_NAME is required to create an image")
        log(f"Stopping instance {ctx.instance} before baking {ctx.baked_image_name}")
        gcloud(
            "compute",
            "instances",
            "stop",
            ctx.instance,
            f"--project={ctx.project}",
            f"--zone={ctx.zone}",
        )
        gcloud(
            "compute",
            "images",
            "create",
            ctx.baked_image_name,
            f"--project={ctx.project}",
            f"--source-disk={ctx.instance}",
            f"--source-disk-zone={ctx.zone}",
        )


def instance_status(ctx: Context) -> str | None:
    result = gcloud(
        "compute",
        "instances",
        "describe",
        ctx.instance,
        f"--project={ctx.project}",
        f"--zone={ctx.zone}",
        "--format=get(status)",
        capture=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip() or None


def external_ip(ctx: Context) -> str | None:
    result = gcloud(
        "compute",
        "instances",
        "describe",
        ctx.instance,
        f"--project={ctx.project}",
        f"--zone={ctx.zone}",
        "--format=get(networkInterfaces[0].accessConfigs[0].natIP)",
        capture=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    ip = result.stdout.strip()
    return ip or None


def wait_for_external_ip(ctx: Context) -> str:
    while True:
        ip = external_ip(ctx)
        if ip:
            log(f"External IP is {ip}")
            return ip
        log("Waiting for external IP...")
        time.sleep(5)


def port_open(host: str | None, port: int, timeout: float = 5) -> bool:
    if not host:
        return False
    try:
        with socket.create_connection((host, port), timeout=timeout):
            return True
    except OSError:
        return False


def wait_for_port(host: str, port: int, attempts: int = 60, delay: int = 10) -> None:
    for _ in range(attempts):
        if port_open(host, port):
            log(f"SSH is reachable on {host}:{port}")
            return
        log(f"Waiting for SSH on {host}:{port}...")
        time.sleep(delay)
    print(f"Timed out waiting for SSH on {host}:{port}", file=sys.stderr)
    raise SystemExit(1)


def ssh_into(ctx: Context, host: str) -> None:
    run("ssh-keygen", "-R", host, check=False)
    log(f"Connecting to {ctx.user}@{host}:{ctx.port}")
    expect_script = f"""
set timeout -1
spawn ssh -o StrictHostKeyChecking=accept-new -o ServerAliveInterval=30 -p {ctx.port} {ctx.user}@{host}
expect {{
  "*?assword:" {{
    send "{ctx.password}\\r"
  }}
  eof {{
    exit 1
  }}
}}
interact
"""
    os.execvp("expect", ["expect", "-c", expect_script])


def ssh_run(ctx: Context, host: str, command: str) -> None:
    log(f"Running remote command: {command}")
    run("ssh-keygen", "-R", host, check=False)
    quoted = shlex.quote(command)
    expect_script = f"""
set timeout -1
spawn ssh -o StrictHostKeyChecking=accept-new -o ServerAliveInterval=30 -p {ctx.port} {ctx.user}@{host} {quoted}
expect {{
  "yes/no" {{
    send "yes\\r"
    exp_continue
  }}
  "*?assword:" {{
    send "{ctx.password}\\r"
    exp_continue
  }}
  eof {{
    catch wait result
    set status [lindex $result 3]
    exit $status
  }}
}}
"""
    run("expect", "-c", expect_script)


def write_startup_script(path: Path, user: str, password: str, port: int, idle_timeout_seconds: int) -> None:
    path.write_text(
        f"""$ErrorActionPreference = "Stop"
$idleScriptPath = "C:\\ProgramData\\auto-shutdown.ps1"
$idleLogPath = "C:\\ProgramData\\auto-shutdown.log"

Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0

if (-not (Get-LocalUser -Name "{user}" -ErrorAction SilentlyContinue)) {{
  $password = ConvertTo-SecureString "{password}" -AsPlainText -Force
  New-LocalUser -Name "{user}" -Password $password -AccountNeverExpires
}}

Add-LocalGroupMember -Group "Administrators" -Member "{user}" -ErrorAction SilentlyContinue

Start-Service sshd
Set-Service -Name sshd -StartupType Automatic
New-ItemProperty -Path "HKLM:\\SOFTWARE\\OpenSSH" -Name DefaultShell -Value "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe" -PropertyType String -Force

if (-not (Get-NetFirewallRule -Name "sshd" -ErrorAction SilentlyContinue)) {{
  New-NetFirewallRule -Name "sshd" -DisplayName "OpenSSH Server" -Enabled True -Direction Inbound -Protocol TCP -Action Allow -LocalPort {port}
}}

@'
$ErrorActionPreference = "Stop"
$timeout = {idle_timeout_seconds}
$interval = 60
$idleSince = Get-Date
$logPath = "C:\\ProgramData\\auto-shutdown.log"

while ($true) {{
  $activeSsh = Get-NetTCPConnection -State Established -LocalPort {port} -ErrorAction SilentlyContinue
  if ($activeSsh) {{
    $idleSince = Get-Date
  }}

  $idleSeconds = [int]((Get-Date) - $idleSince).TotalSeconds
  Add-Content $logPath "$(Get-Date -Format o) active=$(@($activeSsh).Count) idle=$idleSeconds"
  if ($idleSeconds -ge $timeout) {{
    Add-Content $logPath "$(Get-Date -Format o) shutting down"
    Stop-Computer -Force
  }}

  Start-Sleep -Seconds $interval
}}
'@ | Set-Content -Path $idleScriptPath -Encoding UTF8

New-Item -ItemType File -Path $idleLogPath -Force | Out-Null

$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-NoProfile -ExecutionPolicy Bypass -File `"$idleScriptPath`""
$trigger = New-ScheduledTaskTrigger -AtStartup
$principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" -RunLevel Highest
Register-ScheduledTask -TaskName "GhcidAutoShutdown" -Action $action -Trigger $trigger -Principal $principal -Force
Start-ScheduledTask -TaskName "GhcidAutoShutdown"
""",
        encoding="utf-8",
    )


def provision_instance(ctx: Context, host: str) -> None:
    commands = [
        "Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))",
        "choco install -y git",
        "Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -DisableCurl } catch { Write-Error $_ }",
        "$env:PATH += ';C:\\ghcup\\bin'; ghcup install ghc 9.14.1 --set",
        "$env:PATH += ';C:\\ghcup\\bin'; ghcup install cabal 3.16.1.0 --set",
    ]
    for command in commands:
        ssh_run(ctx, host, f'powershell -NoProfile -ExecutionPolicy Bypass -Command "{command}"')


class BakedInstanceResource(Resource):
    name = "instance from baked image"

    def __init__(self, firewall: FirewallRuleResource, image: ImageResource) -> None:
        self.dependencies = (firewall, image)

    def exists(self, ctx: Context) -> bool:
        return instance_status(ctx) is not None

    def create(self, ctx: Context) -> None:
        if not ctx.baked_image_name:
            raise SystemExit("BAKED_IMAGE_NAME is required to create from image")
        gcloud(
            "compute",
            "instances",
            "create",
            ctx.instance,
            f"--project={ctx.project}",
            f"--zone={ctx.zone}",
            f"--machine-type={ctx.machine_type}",
            f"--boot-disk-size={ctx.boot_disk_size}",
            f"--boot-disk-type={ctx.boot_disk_type}",
            f"--image={ctx.baked_image_name}",
            "--tags=ssh",
        )


class ScratchProvisionedInstanceResource(Resource):
    name = "instance from scratch"

    def __init__(self, firewall: FirewallRuleResource) -> None:
        self.dependencies = (firewall,)

    def exists(self, ctx: Context) -> bool:
        return instance_status(ctx) is not None

    def create(self, ctx: Context) -> None:
        gcloud(
            "compute",
            "instances",
            "create",
            ctx.instance,
            f"--project={ctx.project}",
            f"--zone={ctx.zone}",
            f"--machine-type={ctx.machine_type}",
            f"--boot-disk-size={ctx.boot_disk_size}",
            f"--boot-disk-type={ctx.boot_disk_type}",
            f"--image-project={ctx.image_project}",
            f"--image-family={ctx.image_family}",
            "--network-tier=PREMIUM",
            "--maintenance-policy=MIGRATE",
            "--tags=ssh",
            f"--metadata-from-file=windows-startup-script-ps1={ctx.startup_script}",
        )
        host = wait_for_external_ip(ctx)
        wait_for_port(host, ctx.port)
        provision_instance(ctx, host)


def ensure_running_instance(ctx: Context, image_resource: ImageResource, baked_instance: BakedInstanceResource, scratch_instance: ScratchProvisionedInstanceResource) -> str:
    status = instance_status(ctx)
    if status is not None:
        log(f"Found existing instance {ctx.instance} with status {status}")
        if status == "TERMINATED":
            log(f"Starting instance {ctx.instance}")
            gcloud(
                "compute",
                "instances",
                "start",
                ctx.instance,
                f"--project={ctx.project}",
                f"--zone={ctx.zone}",
            )
        host = wait_for_external_ip(ctx)
        wait_for_port(host, ctx.port)
        return host

    if image_resource.exists(ctx):
        baked_instance.ensure(ctx)
    else:
        log(f"Baked image {ctx.baked_image_name or '<unset>'} does not exist; falling back to scratch provisioning")
        scratch_instance.ensure(ctx)
        if ctx.baked_image_name and not image_resource.exists(ctx):
            image_resource.create(ctx)
            log(f"Restarting instance {ctx.instance} after baking image")
            gcloud(
                "compute",
                "instances",
                "start",
                ctx.instance,
                f"--project={ctx.project}",
                f"--zone={ctx.zone}",
            )

    host = wait_for_external_ip(ctx)
    wait_for_port(host, ctx.port)
    return host


def main() -> int:
    if shutil.which("gcloud") is None:
        print("gcloud is required", file=sys.stderr)
        return 1
    if shutil.which("expect") is None:
        print("expect is required", file=sys.stderr)
        return 1

    with tempfile.TemporaryDirectory() as tmpdir:
        startup_script = Path(tmpdir) / "windows-startup-script.ps1"
        ctx = Context(
            project=env("GCP_PROJECT_ID"),
            zone=env("ZONE", "us-central1-a"),
            instance=env("INSTANCE_NAME", "ghcid-win"),
            machine_type=env("MACHINE_TYPE", "e2-standard-4"),
            image_project=env("IMAGE_PROJECT", "windows-cloud"),
            image_family=env("IMAGE_FAMILY", "windows-2025"),
            boot_disk_size=env("BOOT_DISK_SIZE", "100GB"),
            boot_disk_type=env("BOOT_DISK_TYPE", "pd-ssd"),
            user=env("WINDOWS_SSH_USER", "codex"),
            password=env("WINDOWS_SSH_PASSWORD"),
            port=int(env("SSH_PORT", "22")),
            firewall_rule=env("FIREWALL_RULE", "allow-ssh-windows-gcp"),
            idle_timeout_seconds=int(env("IDLE_TIMEOUT_SECONDS", "600")),
            baked_image_name=env("BAKED_IMAGE_NAME", "windows-haskell-v2"),
            startup_script=startup_script,
        )
        write_startup_script(startup_script, ctx.user, ctx.password, ctx.port, ctx.idle_timeout_seconds)

        log(f"Checking fast path for {ctx.instance}")
        host = external_ip(ctx)
        if port_open(host, ctx.port):
            ssh_into(ctx, host)

        firewall = FirewallRuleResource()
        image = ImageResource()
        baked_instance = BakedInstanceResource(firewall, image)
        scratch_instance = ScratchProvisionedInstanceResource(firewall)

        host = ensure_running_instance(ctx, image, baked_instance, scratch_instance)
        ssh_into(ctx, host)


if __name__ == "__main__":
    raise SystemExit(main())
