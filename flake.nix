# vim: ft=nix ts=2 sts=2 sw=2 et
{
  inputs.stddev.url = "github:PoolloverNathan/stddev";
  outputs = {
    self,
    stddev,
  }:
    stddev rec {
      name = "oxium";
      # deps = pkgs: [pkgs.rustc ]
      packages = system: pkgs: {
        default = let
          mf = (pkgs.lib.importTOML ./Cargo.toml).package;
        in
          assert mf.name == name;
          pkgs.rustPlatform.buildRustPackage rec {
            pname = mf.name;
            version = mf.version;
            cargoLock.lockFile = ./Cargo.lock;
            src = pkgs.lib.cleanSource ./.;
            nativeBuildInputs = [pkgs.pkg-config pkgs.nix pkgs.openssl pkgs.openssl.dev];
            PKG_CONFIG_PATH = ["${pkgs.openssl.dev}/lib/pkgconfig/"];
          };
      };
      deps = pkgs: let p = packages pkgs.system pkgs; in p.default.buildInputs ++ p.default.nativeBuildInputs;
    };
}
