let
  config = {
    # We don't care about xpdf's vulnerabilities for now..
    permittedInsecurePackages = [
        "xpdf-4.02" "pdftohtml-4.02"
    ];
  };

in
  import ../reflex-platform {
    inherit config;
    hieSupport = false;
  }