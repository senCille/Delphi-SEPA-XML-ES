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
}

interface

uses System.Generics.Collections,
     senCille.SEPAAuxClasses,
     senCille.CustomSEPA;

type
  TDJMNorma3414XML = class(TCustomSEPA) //el Ordenante paga al Beneficiario
    FInitiators :TList<TsepaInitiator>; //Ordenantes, uno por cada cuenta de cargo
  private
    procedure AddGroupHeader;
    procedure AddOperation  (AOrdenante :TsepaInitiator);
    procedure AddCreditTransferOperation(AOperation :TsepaOperation);

    function GetNumOperations:Integer;
    function GetTotalImport  :Double;
  public
    property Initiators :TList<TsepaInitiator> read FInitiators;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile;
    function ThereAreOperations:Boolean;
  end;

implementation

uses System.SysUtils, System.Classes, Dialogs;

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

procedure TDJMNorma3414XML.AddGroupHeader;
begin
   //1.0 Group Header Conjunto de características compartidas por todas las operaciones incluidas en el mensaje
   AddLine('<GrpHdr>');

   //1.1 MessageId Referencia asignada por la parte iniciadora y enviada a la siguiente
   //parte de la cadena para identificar el mensaje de forma inequívoca
   AddLine('<MsgId>'+CleanStr(GenerateUUID)+'</MsgId>');

   //1.2 Fecha y hora cuando la parte iniciadora ha creado un (grupo de) instrucciones de pago
   //(con 'now' es suficiente)
   AddLine('<CreDtTm>'+FormatDateTimeXML(FileDate)+'</CreDtTm>');

   //1.6  Número de operaciones individuales que contiene el mensaje
   AddLine('<NbOfTxs>'+IntToStr(GetNumOperations)+'</NbOfTxs>');

   //1.7 Suma total de todos los importes individuales incluidos en el mensaje
   AddLine('<CtrlSum>'+FormatAmountXML(GetTotalImport)+'</CtrlSum>');

   //1.8 Parte que presenta el mensaje. En el mensaje de presentación, puede ser el “Ordenante” o “el presentador”
   AddLine('<InitgPty>');
       //Nombre de la parte
       AddLine('<Nm>'+CleanStr(InitiatorName, INITIATOR_NAME_MAX_LENGTH)+'</Nm>');

       //Para el sistema de adeudos SEPA se utilizará exclusivamente la etiqueta “Otra” estructurada
       //según lo definido en el epígrafe “Identificador del presentador” de la sección 3.3
       AddLine('<Id>'                    );
       AddLine('<OrgId>'                 );
       AddLine('<Othr>'                  );
       AddLine('<Id>'+InitiatorId+'</Id>');
       AddLine('</Othr>'                 );
       AddLine('</OrgId>'                );
       AddLine('</Id>'                   );
   AddLine('</InitgPty>');

   AddLine('</GrpHdr>');
end;


procedure TDJMNorma3414XML.SaveToFile;
var i :TsepaInitiator;
begin
   FOutput := TStringList.Create;
   try
       AddLine('<?xml version="1.0" encoding="UTF-8"?>');
       AddLine('<Document xmlns="urn:iso:std:iso:20022:tech:xsd:'+SCHEMA_34+'"'+' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">');

       //MESSAGE ROOT. Identifica el tipo de mensaje
       AddLine('<CstmrCdtTrfInitn>'); //'<CstmrDrctDbtInitn>');
       AddGroupHeader;

       //la info de cada Ordenante
       for i in FInitiators do begin
          if i.Operations.Count > 0 then AddOperation(i);
       end;

       AddLine('</CstmrCdtTrfInitn>');
       AddLine('</Document>');

       FOutput.SaveToFile(FileName);
   finally
      FOutput.Free;
   end;
end;

procedure TDJMNorma3414XML.AddOperation(AOrdenante :TsepaInitiator);
var i :TsepaOperation;
begin
   //2.0 Información del pago - PaymentInformation
   AddLine('<PmtInf>');
   //2.1 Identificación de Información del pago - PaymentInformationIdentification
   //Referencia asignada por el ordenante para identificar claramente el bloque de información de pago dentro del mensaje
   AddLine('<PmtInfId>'+CleanStr(AOrdenante.PaymentId)+'</PmtInfId>');
   //2.2 Método de pago - PaymentMethod
   AddLine('<PmtMtd>'+'TRF'+'</PmtMtd>');
   //2.4 Número de operaciones - NumberOfTransactions
   AddLine('<NbOfTxs>'+IntToStr(AOrdenante.Operations.Count)+'</NbOfTxs>');
   //2.5 Con trol de suma - ControlSum
   //Suma total de todos los importes individuales incluidos en el bloque de información
   //de pago, sin tener en cuenta las divisas. Sirve como elemento de control.
   AddLine('<CtrlSum>'+FormatAmountXML(AOrdenante.GetTotalImport)+'</CtrlSum>');
   //2.6 Información del tipo de pago - PaymentTypeInformation
   AddLine('<PmtTpInf>');
   //2.8 Nivel de servicio - ServiceLevel
   AddLine('<SvcLvl><Cd>'+'SEPA'+'</Cd></SvcLvl>');
   AddLine('</PmtTpInf>');
   //2.17 Fecha de ejecución solicitada - Requested ExecutionDate
   AddLine('<ReqdExctnDt>'+FormatDateXML(ChargeDate)+'</ReqdExctnDt>');
   //2.19 Ordenante - Debtor
   AddLine('<Dbtr><Nm>'+CleanStr(AOrdenante.Name)+'</Nm></Dbtr>');

   //2.20 Cuenta del ordenante - DebtorAccount
   AddLine('<DbtrAcct>');
   AddAccountId(AOrdenante.IBAN);
   AddLine('</DbtrAcct>');

   //2.21 Entidad del ordenante - DebtorAgent
   AddLine('<DbtrAgt>');
   AddBICInfo(AOrdenante.BIC);
   AddLine('</DbtrAgt>');

   //2.24 Cláusula de gastos - ChargeBearer
   //writeLn(FOuputFile, '<ChrgBr>'+uSEPA_CleanString(ChrgBr)+'</ChrgBr>');

   for i in AOrdenante.Operations do begin
      AddCreditTransferOperation(i);
   end;

   AddLine('</PmtInf>');
end;

procedure TDJMNorma3414XML.AddCreditTransferOperation;
begin
   //2.27 Información de tran sferencia individual - CreditTransferTran sactionInformation
   AddLine('<CdtTrfTxInf>');

   //2.28 Identificación del pago - PaymentIdentification
   AddLine('<PmtId>');
   //2.30 Identificación de extremo a extremo - EndTo EndIdentification
   //Referencia única que asigna la parte i niciadora para identi ficar la operación
   //y que se transmite sin cambios a lo largo de la cadena del pago hasta el beneficiario.
   AddLine('<EndToEndId>'+CleanStr(AOperation.OpId)+'</EndToEndId>');
   AddLine('</PmtId>');

   //2.31 Información del tipo de pago – PaymentTypeInformation
   //<PmtTpInf>

   //2.42 Importe - Amoun t
   AddLine('<Amt><InstdAmt Ccy="'+'EUR'+'">'+FormatAmountXML(AOperation.Import)+'</InstdAmt></Amt>');

   //2.77 Entidad del beneficiario - CreditorAgent
   AddLine('<CdtrAgt>');
   AddBICInfo(AOperation.BIC);
   AddLine('</CdtrAgt>');

   //2.79 Beneficiario - Creditor
   AddLine('<Cdtr><Nm>'+CleanStr(AOperation.Name, BENEFICIARIO_NAME_MAX_LENGTH)+'</Nm></Cdtr>');

   //2.80 Cuenta del beneficiario - CreditorAccount
   AddLine('<CdtrAcct>');
   AddAccountId(AOperation.IBAN);
   AddLine('</CdtrAcct>');

   //2.98 Concepto - RemittanceInformation
   AddLine('<RmtInf><Ustrd>'+CleanStr(AOperation.Concept, RMTINF_MAX_LENGTH)+'</Ustrd></RmtInf>');

   AddLine('</CdtTrfTxInf>');
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
