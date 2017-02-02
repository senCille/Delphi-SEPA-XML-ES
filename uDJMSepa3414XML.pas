unit uDJMSepa3414XML;
{
https://github.com/cocosistemas/Delphi-SEPA-XML-ES
Diego J.Muñoz. Freelance. Cocosistemas.com
}
//2016-01-20
//ver los pdfs de los bancos, con la norma.
//34.14 pagos. 'Sepa credit transfer'

//el ORDENANTE le paga al BENEFICIARIO

//Tenemos un array de Ordenantes (**cada uno con un IBAN de cargo**), y para cada Ordenante
//un array con sus ordenes de pago
{
uso:
  Set Initial values for Initiator. Four properties.
- Añadimos Ordenante: addInitiator (uno por cada cuenta de cargo del pago, donde nos cargan lo pagado)
- Añadimos los pagos: addPago (uno por cada pago, el solo se coloca en su Ordenante, éste ha tenido que
ser añadido previamente)
- createfile (las ordenes estan en los arrays)
- closefile
}

interface

uses System.Generics.Collections,
     senCille.SEPAAuxClasses,
     senCille.CustomSEPA;

type
  TDJMNorma3414XML = class(TCustomSEPA) //el Ordenante paga al Beneficiario
    FOuputFile  :Text;
    FInitiators :TList<TsepaInitiator>; //Ordenantes, uno por cada cuenta de cargo
  private
    procedure WriteGroupHeader;
    procedure WriteOrdenesPago(AOrdenante :TsepaInitiator);
    procedure WriteCreditTransferOperationInfo(AOperation :TsepaOperation);

    function GetNumOperations:Integer;
    function GetTotalImport  :Double;
  public
    property Initiators :TList<TsepaInitiator> read FInitiators;
    constructor Create;
    destructor Destroy; override;
    procedure AddInitiator(AInitiator :TsepaInitiator);

    procedure CreateFile(AFileName :string);
    procedure CloseFile;
    function ThereAreOperations:Boolean;
  end;

implementation

uses SysUtils, Windows, Dialogs;

constructor TDJMNorma3414XML.Create;
begin
   inherited;
   FInitiators := TList<TsepaInitiator>.Create;
end;

destructor TDJMNorma3414XML.Destroy;
begin
   FInitiators.Free;
   inherited Destroy;
end;

procedure TDJMNorma3414XML.WriteGroupHeader;
begin
   //1.0 Group Header Conjunto de características compartidas por todas las operaciones incluidas en el mensaje
   Writeln(FOuputFile, '<GrpHdr>');

   //1.1 MessageId Referencia asignada por la parte iniciadora y enviada a la siguiente
   //parte de la cadena para identificar el mensaje de forma inequívoca
   Writeln(FOuputFile, '<MsgId>'+CleanStr(GenerateUUID)+'</MsgId>');

   //1.2 Fecha y hora cuando la parte iniciadora ha creado un (grupo de) instrucciones de pago
   //(con 'now' es suficiente)
   Writeln(FOuputFile, '<CreDtTm>'+FormatDateTimeXML(FileDate)+'</CreDtTm>');

   //1.6  Número de operaciones individuales que contiene el mensaje
   Writeln(FOuputFile, '<NbOfTxs>'+IntToStr(GetNumOperations)+'</NbOfTxs>');

   //1.7 Suma total de todos los importes individuales incluidos en el mensaje
   writeLn(FOuputFile, '<CtrlSum>'+FormatAmountXML(GetTotalImport)+'</CtrlSum>');

   //1.8 Parte que presenta el mensaje. En el mensaje de presentación, puede ser el “Ordenante” o “el presentador”
   Write(FOuputFile, '<InitgPty>');
       //Nombre de la parte
       WriteLn(FOuputFile, '<Nm>'+CleanStr(InitiatorName, INITIATOR_NAME_MAX_LENGTH)+'</Nm>');

       //Para el sistema de adeudos SEPA se utilizará exclusivamente la etiqueta “Otra” estructurada
       //según lo definido en el epígrafe “Identificador del presentador” de la sección 3.3
       WriteLn(FOuputFile, '<Id>'                    );
       WriteLn(FOuputFile, '<OrgId>'                 );
       WriteLn(FOuputFile, '<Othr>'                  );
       WriteLn(FOuputFile, '<Id>'+InitiatorId+'</Id>');
       WriteLn(FOuputFile, '</Othr>'                 );
       WriteLn(FOuputFile, '</OrgId>'                );
       WriteLn(FOuputFile, '</Id>'                   );
   Writeln(FOuputFile,'</InitgPty>');

   Writeln(FOuputFile, '</GrpHdr>');
end;


procedure TDJMNorma3414XML.CreateFile(AFileName :string);
var i :TsepaInitiator;
begin
   AssignFile(FOuputFile, AFileName);
   Rewrite(FOuputFile);
   WriteLn(FOuputFile, '<?xml version="1.0" encoding="UTF-8"?>');

   WriteLn(FOuputFile,'<Document xmlns="urn:iso:std:iso:20022:tech:xsd:'+SCHEMA_34+'"'+
                     ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">');

   //MESSAGE ROOT. Identifica el tipo de mensaje
   WriteLn(FOuputFile, '<CstmrCdtTrfInitn>'); //'<CstmrDrctDbtInitn>');
   WriteGroupHeader;

   //la info de cada Ordenante
   for i in FInitiators do begin
      if i.Operations.Count > 0 then WriteOrdenesPago(i);
   end;

   WriteLn(FOuputFile, '</CstmrCdtTrfInitn>');
   WriteLn(FOuputFile, '</Document>');
end;

procedure TDJMNorma3414XML.CloseFile;
begin
   Close(FOuputFile);
end;

procedure TDJMNorma3414XML.WriteOrdenesPago(AOrdenante :TsepaInitiator);
var i :TsepaOperation;
begin
   //2.0 Información del pago - PaymentInformation
   WriteLn(FOuputFile, '<PmtInf>');
   //2.1 Identificación de Información del pago - PaymentInformationIdentification
   //Referencia asignada por el ordenante para identificar claramente el bloque de información de pago dentro del mensaje
   WriteLn(FOuputFile, '<PmtInfId>'+CleanStr(AOrdenante.PaymentId)+'</PmtInfId>');
   //2.2 Método de pago - PaymentMethod
   WriteLn(FOuputFile, '<PmtMtd>'+'TRF'+'</PmtMtd>');
   //2.4 Número de operaciones - NumberOfTransactions
   WriteLn(FOuputFile, '<NbOfTxs>'+IntToStr(AOrdenante.Operations.Count)+'</NbOfTxs>');
   //2.5 Con trol de suma - ControlSum
   //Suma total de todos los importes individuales incluidos en el bloque de información
   //de pago, sin tener en cuenta las divisas. Sirve como elemento de control.
   WriteLn(FOuputFile, '<CtrlSum>'+FormatAmountXML(AOrdenante.GetTotalImport)+'</CtrlSum>');
   //2.6 Información del tipo de pago - PaymentTypeInformation
   WriteLn(FOuputFile, '<PmtTpInf>');
   //2.8 Nivel de servicio - ServiceLevel
   WriteLn(FOuputFile, '<SvcLvl><Cd>'+'SEPA'+'</Cd></SvcLvl>');
   WriteLn(FOuputFile, '</PmtTpInf>');
   //2.17 Fecha de ejecución solicitada - Requested ExecutionDate
   WriteLn(FOuputFile, '<ReqdExctnDt>'+FormatDateXML(ChargeDate)+'</ReqdExctnDt>');
   //2.19 Ordenante - Debtor
   WriteLn(FOuputFile, '<Dbtr><Nm>'+CleanStr(AOrdenante.Name)+'</Nm></Dbtr>');

   //2.20 Cuenta del ordenante - DebtorAccount
   WriteLn(FOuputFile, '<DbtrAcct>');
   WriteAccountIdentification(FOuputFile, AOrdenante.IBAN);
   WriteLn(FOuputFile, '</DbtrAcct>');

   //2.21 Entidad del ordenante - DebtorAgent
   WriteLn(FOuputFile, '<DbtrAgt>');
   WriteBICInfo(FOuputFile, AOrdenante.BIC);
   WriteLn(FOuputFile, '</DbtrAgt>');

   //2.24 Cláusula de gastos - ChargeBearer
   //writeLn(FOuputFile, '<ChrgBr>'+uSEPA_CleanString(ChrgBr)+'</ChrgBr>');

   for i in AOrdenante.Operations do begin
      WriteCreditTransferOperationInfo(i);
   end;

   WriteLn(FOuputFile, '</PmtInf>');
end;

procedure TDJMNorma3414XML.WriteCreditTransferOperationInfo;
begin
   //2.27 Información de tran sferencia individual - CreditTransferTran sactionInformation
   WriteLn(FOuputFile, '<CdtTrfTxInf>');

   //2.28 Identificación del pago - PaymentIdentification
   Write(FOuputFile, '<PmtId>');
   //2.30 Identificación de extremo a extremo - EndTo EndIdentification
   //Referencia única que asigna la parte i niciadora para identi ficar la operación
   //y que se transmite sin cambios a lo largo de la cadena del pago hasta el beneficiario.
   Write(FOuputFile, '<EndToEndId>'+CleanStr(AOperation.OpId)+'</EndToEndId>');
   WriteLn(FOuputFile, '</PmtId>');

   //2.31 Información del tipo de pago – PaymentTypeInformation
   //<PmtTpInf>

   //2.42 Importe - Amoun t
   WriteLn(FOuputFile, '<Amt><InstdAmt Ccy="'+'EUR'+'">'+FormatAmountXML(AOperation.Import)+'</InstdAmt></Amt>');

   //2.77 Entidad del beneficiario - CreditorAgent
   WriteLn(FOuputFile, '<CdtrAgt>');
   WriteBICInfo(FOuputFile, AOperation.BIC);
   WriteLn(FOuputFile, '</CdtrAgt>');

   //2.79 Beneficiario - Creditor
   WriteLn(FOuputFile, '<Cdtr><Nm>'+CleanStr(AOperation.Name, BENEFICIARIO_NAME_MAX_LENGTH)+'</Nm></Cdtr>');

   //2.80 Cuenta del beneficiario - CreditorAccount
   WriteLn(FOuputFile, '<CdtrAcct>');
   WriteAccountIdentification(FOuputFile, AOperation.IBAN);
   WriteLn(FOuputFile, '</CdtrAcct>');

   //2.98 Concepto - RemittanceInformation
   WriteLn(FOuputFile, '<RmtInf><Ustrd>'+CleanStr(AOperation.Concept, RMTINF_MAX_LENGTH)+'</Ustrd></RmtInf>');

   WriteLn(FOuputFile, '</CdtTrfTxInf>');
end;

procedure TDJMNorma3414XML.AddInitiator(AInitiator :TsepaInitiator);
begin
  FInitiators.Add(AInitiator);
end;

function TDJMNorma3414XML.GetNumOperations:Integer;
var i :TsepaInitiator;
begin
   Result := 0;
   for i in FInitiators do begin
      Result := Result + i.Operations.Count;
   end;
end;

function TDJMNorma3414XML.GetTotalImport:Double;
var i :TsepaInitiator;
begin
   Result := 0;
   for i in FInitiators do begin
      Result := Result + i.GetTotalImport;
   end;
end;

function TDJMNorma3414XML.ThereAreOperations;
begin
   Result := GetTotalImport <> 0;
end;

end.
