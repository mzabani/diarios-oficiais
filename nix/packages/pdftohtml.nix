{ pkgs ? import ../nixpkgs.nix {} }:
    (pkgs.xpdf.override { enableGUI = false; enablePDFtoPPM = false; enablePrinting = false; }).overrideAttrs (old: {
            pname = "pdftohtml";
            nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.qt5.qmake ];
            postInstall = old.postInstall + ''
                rm -f $out/bin/pdfdetach $out/bin/pdffonts $out/bin/pdfimages $out/bin/pdfinfo $out/bin/pdftopng $out/bin/pdftoppm $out/bin/pdftops $out/bin/pdftotext $out/bin/xpdf
            '';
        })