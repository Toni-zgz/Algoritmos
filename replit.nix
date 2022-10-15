{ pkgs }: {
    deps = [
        pkgs.dotnet-sdk
        pkgs.sgtpuzzles
        pkgs.clojure
        pkgs.clojure-lsp
    ];
}