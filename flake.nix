{
  description = "flake for Coachtaal compiler";

  inputs = {
	  nixpkgs.url = "github:NixOS/nixpkgs/9b5328b7f761a7bbdc0e332ac4cf076a3eedb89b";
  };

  outputs = { self, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs {
			inherit system;
		};
      });
    in
    {
	  packages = forEachSupportedSystem({ pkgs }: with pkgs; {
		  default = (let
			  buildMavenRepo = callPackage ./nix/maven-repo.nix { };

			  mavenRepo = buildMavenRepo {
				name = "nix-maven-repo";
				repos = [
				  "https://repo1.maven.org/maven2"
				  "https://plugins.gradle.org/m2"
				  "https://maven.pkg.jetbrains.space/kotlin/p/kotlin/dev"
				];
				deps = builtins.fromJSON (builtins.readFile ./deps.json);
			  };
		in pkgs.stdenv.mkDerivation {
			  pname = "coach";
			  version = "0.1";
			  src = ./.;
			  buildInputs = with pkgs; [
			  jdk
			  gradle
			  makeWrapper
			  ];
			  JDK_HOME = "${jdk.home}";

			  buildPhase = ''
			  runHook preBuild
			  export GRADLE_USER_HOME=$TMP/gradle-home
			  export NIX_MAVEN_REPO=${mavenRepo}
			  gradle :app:installDist :lsp:installDist -x test \
				  --offline --no-daemon \
				  --warning-mode=all --parallel --console=plain \
				  -PnixMavenRepo=${mavenRepo}
			  runHook postBuild
				  '';

			  installPhase = ''
				  runHook preInstall
				  mkdir -p $out
				  cp -r app/build/install/coach/* $out
				  cp -r --no-clobber lsp/build/install/coach-lsp/* $out

				  wrapProgram $out/bin/coach \
				  --set "JAVA_HOME" "${pkgs.jdk}"

				  wrapProgram $out/bin/coach-lsp \
				  --set "JAVA_HOME" "${pkgs.jdk}"

				  runHook postInstall
				  '';

			  dontStrip = true;
			  meta = {
				  description = "Coachtaal compiler";
			  };
		  });
	  });

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            gradle
            kotlin
			(pkgs.callPackage ./nix/update-locks.nix {})
          ];
        };
      });
    };
}
