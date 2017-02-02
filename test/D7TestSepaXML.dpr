program D7TestSepaXML;

uses
  Forms,
  fMain in 'fMain.pas' {frMain},
  uDJMSepa1914XML in '..\uDJMSepa1914XML.pas',
  uDJMSepa3414XML in '..\uDJMSepa3414XML.pas',
  senCille.CustomSEPA in '..\senCille.CustomSEPA.pas',
  senCille.SEPAAuxClasses in '..\senCille.SEPAAuxClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
