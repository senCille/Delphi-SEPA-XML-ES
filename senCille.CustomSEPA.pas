unit senCille.CustomSEPA;

   {https://github.com/sencille/Delphi-SEPA-XML-ES
    Juan C.Cilleruelo Gonzalo. senCille.es
    Based on a previous version donated by:
          https://github.com/cocosistemas/Delphi-SEPA-XML-ES
          Diego J.Muñoz. Freelance. Cocosistemas.com         }

interface

uses System.Classes;

type
  TCustomSEPA = class
  private
    FFileName      :string;    {Fisical file name}
    FFileDate      :TDateTime; {Date of presentation for the file}
    FInitiatorName :string;    {Name of the presenter ('initiator')}
    FInitiatorID   :string;    {Id presentador norma AT02}
    FChargeDate    :TDateTime; {Date of charge into account}
  protected
    FOutput :TStringList;
    procedure AddLine(AText :string);
  const
    SCHEMA_19                    = 'pain.008.001.02';
    SCHEMA_34                    = 'pain.001.001.03';
    INITIATOR_NAME_MAX_LENGTH    =  70;
    BENEFICIARIO_NAME_MAX_LENGTH =  70;
    DEUDOR_NAME_MAX_LENGTH       =  70;
    ORDENANTE_NAME_MAX_LENGTH    =  70;
    RMTINF_MAX_LENGTH            = 140;
    MNDTID_MAX_LENGTH            =  35;

    function  CleanStr(AString :string; ALength :Integer = -1):string;
    procedure AddAccountId(AIBAN :string);
    procedure AddBICInfo  (ABIC  :string);
    function  GenerateUUID:string;
    function  FormatDateTimeXML(const ADateTime :TDateTime                          ):string;
    function  FormatAmountXML  (const ACurrency :Currency; const Digits :Integer = 2):string;
    function  FormatDateXML    (const ADateTime :TDateTime                          ):string;
  public
    constructor Create;
    property FileName      :string    read FFileName      write FFileName;
    property FileDate      :TDateTime read FFileDate      write FFileDate;
    property InitiatorName :string    read FInitiatorName write FInitiatorName;
    property InitiatorId   :string    read FInitiatorId   write FInitiatorId;
    property ChargeDate    :TDateTime read FChargeDate    write FChargeDate;
  end;

implementation

uses System.SysUtils, System.Math;

constructor TCustomSEPA.Create;
begin
   inherited;
   FFileDate   := Now;
   FChargeDate := Now + 10;
end;

procedure TCustomSEPA.AddLine(AText: string);
begin
   FOutput.Add(AText);
end;

function TCustomSEPA.CleanStr(AString :string; ALength :Integer = -1):string;
var i :Integer;
begin
   Result := AString;
   Result := StringReplace(Result, 'á', 'a', [rfReplaceAll]);
   Result := StringReplace(Result, 'Á', 'A', [rfReplaceAll]);
   Result := StringReplace(Result, 'é', 'e', [rfReplaceAll]);
   Result := StringReplace(Result, 'É', 'E', [rfReplaceAll]);
   Result := StringReplace(Result, 'í', 'i', [rfReplaceAll]);
   Result := StringReplace(Result, 'Í', 'I', [rfReplaceAll]);
   Result := StringReplace(Result, 'ó', 'o', [rfReplaceAll]);
   Result := StringReplace(Result, 'Ó', 'O', [rfReplaceAll]);
   Result := StringReplace(Result, 'ú', 'u', [rfReplaceAll]);
   Result := StringReplace(Result, 'Ú', 'U', [rfReplaceAll]);
   Result := StringReplace(Result, 'Á', 'A', [rfReplaceAll]);
   Result := StringReplace(Result, 'é', 'e', [rfReplaceAll]);
   Result := StringReplace(Result, 'É', 'E', [rfReplaceAll]);
   Result := StringReplace(Result, 'í', 'i', [rfReplaceAll]);
   Result := StringReplace(Result, 'Í', 'I', [rfReplaceAll]);
   Result := StringReplace(Result, 'ó', 'o', [rfReplaceAll]);
   Result := StringReplace(Result, 'Ó', 'O', [rfReplaceAll]);
   Result := StringReplace(Result, 'ú', 'u', [rfReplaceAll]);
   Result := StringReplace(Result, 'Ú', 'U', [rfReplaceAll]);
   Result := StringReplace(Result, 'Ö', 'O', [rfReplaceAll]);
   Result := StringReplace(Result, 'ö', 'o', [rfReplaceAll]);
   Result := StringReplace(Result, 'Ñ', 'N', [rfReplaceAll]);
   Result := StringReplace(Result, 'ñ', 'n', [rfReplaceAll]);
   Result := StringReplace(Result, 'Ç', 'C', [rfReplaceAll]);
   Result := StringReplace(Result, 'ç', 'c', [rfReplaceAll]);

   {Change all unallowed characters by a space}
   for i := 1 to Length(Result) do begin
      if not(Ord(Result[i]) in [65..90, 97..122, 48..57, 47, 45, 63, 58, 40, 41, 46, 44, 39, 43, 32]) then Result[i] := ' ';
   end;
   // Convertir a mayúsculas
   //Result := AnsiUpperCase(Result);

   // Codificar a UTF8
   Result := Utf8Encode(Trim(Result));
   if (ALength >= 0) and (Length(Result) > ALength) then Result := Copy(Result, 1, ALength);
end;

procedure TCustomSEPA.AddAccountId(AIBAN :string);
begin
   AddLine('<Id><IBAN>'+CleanStr(AIBAN)+'</IBAN></Id>');
end;

procedure TCustomSEPA.AddBICInfo(ABIC :string);
begin
   AddLine('<FinInstnId><BIC>'+CleanStr(ABIC)+'</BIC></FinInstnId>');
end;

function TCustomSEPA.GenerateUUID:string;
var UId :TGuid;
    Res :HResult;
begin
   Res := CreateGuid(Uid);
   if Res = S_OK then begin
      Result := GuidToString(UId);
      Result := StringReplace(Result, '-', '', [rfReplaceAll]);
      Result := StringReplace(Result, '{', '', [rfReplaceAll]);
      Result := StringReplace(Result, '}', '', [rfReplaceAll]);
   end
   else Result := IntToStr(RandomRange(10000, High(Integer)));  // fallback to simple random number
end;

function TCustomSEPA.FormatDateXML(const ADateTime :TDateTime):string;
begin
   Result := FormatDateTime('yyyy"-"mm"-"dd', ADateTime);
end;

function TCustomSEPA.FormatAmountXML(const ACurrency :Currency; const Digits: Integer = 2):string;
var FS              :TFormatSettings;
    OldDecSeparator :Char;
begin
   OldDecSeparator := FS.DecimalSeparator;
   FS.DecimalSeparator := '.';
   Result := CurrToStrF(ACurrency, ffFixed, Digits);
   FS.DecimalSeparator := OldDecSeparator;
end;

function TCustomSEPA.FormatDateTimeXML(const ADateTime :TDateTime):string;
begin
   Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"', ADateTime);
end;

end.
