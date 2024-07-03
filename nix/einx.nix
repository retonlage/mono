pyPkgs:
  pyPkgs.buildPythonPackage rec {
    pname = "einx";
    version = "0.3.0";
    doCheck = false;
    src = pyPkgs.fetchPypi {
      inherit pname version;
    };
  }
