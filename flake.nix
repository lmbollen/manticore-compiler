{
  description = "Manticore compiler (masm) build toolchain";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # Verilator 4.x is REQUIRED, not just preferred: chiseltest 0.5.x targets it, and
    # the yosys-frontend unit tests' reference compile passes `-Os`, which Verilator 5
    # rejects as a fatal deprecation error ("%Error: Exiting due to 1 warning(s)").
    # nixos-22.11 actually ships Verilator 5.002 (despite the old comment) — pin 22.05,
    # which provides a genuine Verilator 4.222.
    nixpkgs-v4.url = "github:NixOS/nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs, nixpkgs-v4, flake-utils }: flake-utils.lib.eachDefaultSystem (
    system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        verilator4 = (import nixpkgs-v4 { inherit system; }).verilator;

        # Scala 2.13 / firrtl is happiest on a JDK <= 11. Use the prebuilt Eclipse
        # Temurin binary rather than nixpkgs' source-built OpenJDK: the latter SIGSEGVs
        # inside libjvm.so on this host (crash in coursier's dep-fetch threads). The
        # Adoptium binary is stable here. (See manticore-hw/flake.nix.)
        jdk = pkgs.temurin-bin-11;
        sbt = pkgs.sbt.override { jre = jdk; };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Scala build toolchain
            sbt
            jdk
            pkgs.scala_2_13

            # local RTL simulation for the chisel integration tests (chiseltest /
            # verilator backend).
            verilator4

            # helper tooling
            pkgs.python3
            pkgs.jq
            pkgs.which
            pkgs.gnumake

            # sbt/coursier need these to fetch dependencies over https
            pkgs.git
            pkgs.cacert

            # non-interactive bash for the shellHook
            pkgs.bashInteractive
          ];

          # com.google.ortools:ortools-java (used by CustomLutInsertion / the
          # `placement` cone-cover path) ships a bundled libjniortools.so that
          # dlopen()s the C++ runtime. It needs libstdc++ at *runtime*, but we must
          # NOT put it on LD_LIBRARY_PATH globally: doing so can make the JVM load a
          # mismatched libstdc++/libgcc and crash (SIGSEGV in libjvm.so). Expose the
          # path under a separate var and let ortools invocations opt in via
          # LD_LIBRARY_PATH only when they run. e.g.:
          #   LD_LIBRARY_PATH=$ORTOOLS_NATIVE_LIBS sbt "testOnly ...chisel.ANDTester"
          # (tests passing --no-cf skip the cone cover and don't need this.)
          # MASM_ROOT: the yosys frontend (YosysRunner) shells out to
          # $MASM_ROOT/frontend/v2masmyosys (the prebuilt Yosys 0.16). The `masm` script
          # sets this for CLI runs; export it here so `sbt test` (the yosys unit tests)
          # finds it too. $PWD = the repo root when `nix develop` is run from it.
          shellHook = ''
            export LC_ALL="C.UTF-8";
            export JAVA_HOME="${jdk}";
            export ORTOOLS_NATIVE_LIBS="${pkgs.stdenv.cc.cc.lib}/lib:${pkgs.zlib}/lib";
            export MASM_ROOT="$PWD";
          '';
        };
      }
  );
}
