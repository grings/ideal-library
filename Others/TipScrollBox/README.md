# TipVertScrollBox & TipHorzScrollBox

TipVertScrollBox and TipHorzScrollBox controls that solve several problems of native Delphi scrollbox controls, in addition to implementing several optimizations.
These controls internally implement a minimal and modified version (improvement and corrections) of the scrollbox from the Alcinoe library, but creating a weak dependency, that is, when using these scrollbox,
it is possible to enable the use of Alcinoe by adding the define "ALCINOE" to the project, as well as using the native Delphi scrollbox by not declaring this define in the project. So, using TipVertScrollBox and TipHorzScrollBox, at any time you can disable the use of Alcinoe and use the Delphi native scrollbox, without changing any form or control, just configuring it in your project options.
There are two major benefits to creating this weak dependency on third-party libs:
  - Don't get stuck with old versions of Delphi;
  - No need to install third-party libraries. In this specific case, it is neither necessary nor recommended to install Alcinoe, as the compilation and installation of the component will only use Delphi's native scrollbox. Then, in your project options, you will optionally enable the use of Alcinoe through the define ALCINOE, as said before, and adding the source of alcinoe to the project's search path.


## Enabling Alcinoe in project

Project changes to use Alcinoe as the basis of TipVertScrollBox and TipHorzScrollBox:

1. In your project options, add to the search path (IN THIS ORDER):

  `.\TipScrollBox`
  `.\TipScrollBox\alcinoe-min`
  `.\TipScrollBox\fixes\delphi-11`     <<<< AJUST THIS TO YOUR CURRENT DELPHI VERSION
  `$(BDS)\source\fmx`

2. In your project options, add the defines:

  `ALCINOE`
  `DELPHI_FIXES`

3. Add to your project the files: (This is not necessary in Delphi 11 Alexandria+)

  `.\TipScrollBox\iPub.FMX.HorzScrollBox.Base.pas`
  `.\TipScrollBox\iPub.FMX.VertScrollBox.Base.pas`


## Compatible

Delphi 10.4 and 11, in all platforms.

## Author

https://github.com/viniciusfbb
