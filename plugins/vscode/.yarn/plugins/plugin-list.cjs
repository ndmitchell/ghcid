module.exports = {
  name: "plugin-list",
  factory: (require) => {
    const fs = require("fs");
    const { BaseCommand } = require("@yarnpkg/cli");
    const { Command, Option } = require("clipanion");
    const { parseSyml } = require("@yarnpkg/parsers");

    class ListCommand extends BaseCommand {
      static paths = [["list"]];

      static usage = Command.Usage({
        description: "Lists installed packages.",
      });

      prod = Option.Boolean("--prod", false);
      json = Option.Boolean("--json", false);

      async execute() {
        if (!this.prod || !this.json) {
          throw new Error(
            "This command can only be used with the --prod and --json " +
              "args to match the behavior required by VSCE. See: " +
              "https://github.com/microsoft/vscode-vsce/blob/main/src/npm.ts",
          );
        }

        const packageJsonContents = fs.readFileSync("package.json", "utf-8");
        const { dependencies = {}, resolutions = {} } =
          JSON.parse(packageJsonContents);

        const lockContents = fs.readFileSync("yarn.lock", "utf-8");
        const resolved = parseSyml(lockContents);

        const trees = [];

        function addDependency(packageName, versionRange) {
          const packageInfo = lookup(
            resolved,
            getLockFileKey(packageName, versionRange, resolutions),
          );
          if (!packageInfo) {
            throw new Error(
              `Cannot resolve "${packageName}" with version range "${versionRange}"`,
            );
          }

          const { version, dependencies } = packageInfo;
          const name = `${packageName}@${version}`;
          if (trees.find((tree) => tree.name === name)) {
            return; // Dependency already added as part of another tree.
          }

          if (dependencies) {
            const children = Object.entries(dependencies).map(
              ([name, range]) => ({ name: `${name}@${range}` }),
            );
            trees.push({ name, children });

            addDependencies(dependencies);
          } else {
            trees.push({ name, children: [] });
          }
        }

        function addDependencies(dependencies) {
          for (const [packageName, versionRange] of Object.entries(
            dependencies,
          )) {
            addDependency(packageName, versionRange);
          }
        }

        addDependencies(dependencies);

        const output = {
          type: "tree",
          data: { type: "list", trees },
        };

        this.context.stdout.write(JSON.stringify(output));
      }
    }

    return {
      commands: [ListCommand],
    };
  },
};

function getLockFileKey(packageName, versionSpecifier, resolutions) {
  // If the package name is in the resolutions field, use the version from there.
  const resolvedVersionSpecifier = resolutions[packageName] ?? versionSpecifier;

  // Only plain registry semver/tag specifiers use the npm: protocol in the lockfile.
  return usesNpmProtocol(resolvedVersionSpecifier)
    ? `${packageName}@npm:${resolvedVersionSpecifier}`
    : `${packageName}@${resolvedVersionSpecifier}`;
}

function usesNpmProtocol(versionSpecifier) {
  // Protocol-based specifiers (workspace:, patch:, file:, etc.) and git/GitHub
  // references are stored in the lockfile without an added npm: prefix.
  return !(
    versionSpecifier.includes(":") ||
    versionSpecifier.startsWith("github:") ||
    (versionSpecifier.includes("/") && versionSpecifier.includes("#"))
  );
}

/**
 * @param resolved All the resolved dependencies as found in the lock file.
 * @param dependencyKey Key of the dependency to look up. Can be created using
 *                      `getLockFileKey()`.
 */
function lookup(resolved, dependencyKey) {
  const packageInfo = resolved[dependencyKey];
  if (packageInfo) {
    return packageInfo;
  }

  // Fall back to slower iteration-based lookup for combined keys.
  for (const [key, packageInfo] of Object.entries(resolved)) {
    // Resolving ranges: "@babel/runtime@npm:^7.1.2, @babel/runtime@npm:^7.12.13, @babel/runtime@npm:^7.12.5"
    const versionRanges = key.split(",");
    if (versionRanges.some((key) => key.trim() === dependencyKey)) {
      return packageInfo;
    }

    // Resolving yarn link resolutions: "@kaoto/kaoto-ui@portal:/home/rmartinez/repos/kaoto-ui::locator=vscode-kaoto%40workspace%3A."
    const yarnLinkResolution = key.split("::")[0];
    if (yarnLinkResolution === dependencyKey) {
      return packageInfo;
    }
  }
}
