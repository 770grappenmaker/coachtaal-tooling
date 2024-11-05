{
  description = "flake for Coachtaal compiler";

  inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/9b5328b7f761a7bbdc0e332ac4cf076a3eedb89b";
  build-gradle-application.url = "github:dtomvan/buildGradleApplication";
  nasty-jvm-util = {
	  url = "git+file:nasty-jvm-util";
	  flake = false;
  };
  };

  outputs = { self, nixpkgs, build-gradle-application, nasty-jvm-util }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs {
			inherit system;
			overlays = [
				build-gradle-application.overlays.default
			];
		};
      });
    in
    {
	  packages = forEachSupportedSystem({ pkgs }: with pkgs; {
		  default = (buildGradleApplication {
			  pname = "coach";
			  version = "0.1";
			  src = ./.;
			  buildTask = "installDist";
			  dependencyFilter = depSpec:
# kotlinx-serialization-core-metadata-x.y.z.jar is not uploaded to m2...
			  !(
			  (
				  depSpec.component.group
				  == "org.jetbrains.kotlinx"
				  && lib.strings.match ''^kotlinx-serialization-.*-metadata-[0-9]+\.[0-9]+\.[0-9]+\.jar$'' depSpec.name != null
			  ) ||
			  (
				  depSpec.component.group
				  == "org.jetbrains.kotlinx"
				  && depSpec.component.name == "atomicfu"
				  && lib.strings.match ''^atomicfu-metadata-[0-9]+\.[0-9]+\.[0-9]+\.jar$'' depSpec.name != null
			  )
			   );
			  meta = {
				  description = "Coachtaal compiler";
			  };
		  }).overrideAttrs {
			  configurePhase = ''
			  set -x
			  cp -r ${nasty-jvm-util} ./nasty-jvm-util
			  set +x
			  '';
		  };
	  });

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
			updateVerificationMetadata
            gradle
            kotlin
          ];
        };
      });
    };
}
