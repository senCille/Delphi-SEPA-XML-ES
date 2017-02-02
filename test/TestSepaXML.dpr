program TestSepaXML;

uses
  Forms,
  fMain in 'fMain.pas' {frMain},
  senCille.SEPA1914XML in '..\senCille.SEPA1914XML.pas',
  senCille.SEPA3414XML in '..\senCille.SEPA3414XML.pas',
  senCille.CustomSEPA in '..\senCille.CustomSEPA.pas',
  senCille.SEPAAuxClasses in '..\senCille.SEPAAuxClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
