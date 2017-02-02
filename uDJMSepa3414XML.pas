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
setInfoPresentador
- Añadimos Ordenante: addOrdenante (uno por cada cuenta de cargo del pago, donde nos cargan lo pagado)
- Añadimos los pagos: addPago (uno por cada pago, el solo se coloca en su Ordenante, éste ha tenido que
ser añadido previamente)
- createfile (las ordenes estan en los arrays)
- closefile
}

interface

uses senCille.CustomSEPA;

type
  //info de una orden de pago (norma 34.14 xml)
  TInfoPago = class
    sIdPago             :string; //id unico pago, ejemplo:20130930Fra.509301
    mImporte            :Double;
    sBICBeneficiario    :string;
    sNombreBeneficiario :string;
    sIBANBeneficiario   :string;
    sConcepto           :string;
  end;

  TListOfPagos = array[1..5000] of TInfoPago;

  //un conjunto de pagos por Ordenante, lo utilizamos por si utilizan
  //pagos a cargar en diferentes cuentas (el <PmtInf> contiene la info del Ordenante, con su cuenta; y los
  //pagos relacionados con este Ordenante/cuenta de cargo
  TInfoOrdenante = class
    sPayMentId       :string; //Ejemplo: 2013-10-28_095831Remesa 218 UNICO POR Ordenante
    mSumaImportes    :Double;
    sNombreOrdenante :string;
    sIBANOrdenante   :string;
    sBICOrdenante    :string;
    listPagos        :TListOfPagos;
    iPagos           :Integer;
  end;

  TListOrdenantes = array[1..10] of TInfoOrdenante;

  TDJMNorma3414XML = class(TCustomSEPA) //el Ordenante paga al Beneficiario
    FsFileName      :string;
    FsTxt           :Text;
    FiOrdenantes    :Integer;
    FListOrdenantes :TListOrdenantes; //Ordenantes, uno por cada cuenta de cargo

    FdFileDate          :TDateTime; //fecha del fichero
    FmTotalImportes     :Double;    //suma de los importes de los pagos
    FsNombrePresentador :string;    // nombre del presentador (el 'initiator')
    FsIdPresentador     :string;    //id presentador norma AT02
    FdOrdenesPago       :TDateTime; //fecha del cargo en cuenta, PARA TODAS LAS ORDENES

  private
    procedure WriteGroupHeader;
    procedure WriteOrdenesPago(oOrdenante :TInfoOrdenante);
    procedure WriteCreditTransferOperationInfo(oPago :TInfoPago);

    function CalculateNumOperaciones:Integer;
  public
    property iOrdenantes    :Integer         read FiOrdenantes;
    property listOrdenantes :TListOrdenantes read FListOrdenantes;
    constructor Create;
    destructor Destroy; reintroduce;
    procedure SetInfoPresentador(dFileDate          :TDateTime;
                                 sNombrePresentador :string;
                                 sIdPresentador     :string;
                                 dOrdenesPago       :TDateTime);
    procedure AddOrdenante(sPayMentId       :string;
                           sNombreOrdenante :string;
                           sIBANOrdenante   :string;
                           sBICOrdenante    :string);

    procedure AddPago(sIdPago             :string; //id unico pago, ejemplo:20130930Fra.509301
                      mImporte            :Double;
                      sBICBeneficiario    :string;
                      sNombreBeneficiario :string;
                      sIBANBeneficiario   :string;
                      sConcepto           :string;
                      sIBANOrdenante      :string); //el pago lo colocamos en la info de su Ordenante, por la cuenta
    procedure CreateFile(AFileName :string);
    procedure CloseFile;
    function HayPagos :Boolean;
    //pone a cero listas e importe
    procedure Clear;
  end;

implementation
uses SysUtils, Windows, Dialogs;


constructor TDJMNorma3414XML.Create;
begin
   FiOrdenantes        := 0;
   FdFileDate          := Now;
   FmTotalImportes     := 0;
   FsNombrePresentador := '';
   FsIdPresentador     := '';
   FdOrdenesPago       := Now;
end;

procedure TDJMNorma3414XML.SetInfoPresentador;
begin
   FdFileDate          := dFileDate;
   FsNombrePresentador := sNombrePresentador;
   FsIdPresentador     := sIdPresentador;
   FdOrdenesPago       := dOrdenesPago;
end;

destructor TDJMNorma3414XML.Destroy;
begin
   Self.Clear;
   inherited Destroy;
end;

procedure TDJMNorma3414XML.WriteGroupHeader;
begin
   //1.0 Group Header Conjunto de características compartidas por todas las operaciones incluidas en el mensaje
   Writeln(FsTxt, '<GrpHdr>');

   //1.1 MessageId Referencia asignada por la parte iniciadora y enviada a la siguiente
   //parte de la cadena para identificar el mensaje de forma inequívoca
   Writeln(FsTxt, '<MsgId>'+CleanStr(GenerateUUID)+'</MsgId>');

   //1.2 Fecha y hora cuando la parte iniciadora ha creado un (grupo de) instrucciones de pago
   //(con 'now' es suficiente)
   Writeln(FsTxt, '<CreDtTm>'+FormatDateTimeXML(FdFileDate)+'</CreDtTm>');

   //1.6  Número de operaciones individuales que contiene el mensaje
   Writeln(FsTxt, '<NbOfTxs>'+IntToStr(CalculateNumOperaciones)+'</NbOfTxs>');

   //1.7 Suma total de todos los importes individuales incluidos en el mensaje
   writeLn(FsTxt, '<CtrlSum>'+FormatAmountXML(FmTotalImportes)+'</CtrlSum>');

   //1.8 Parte que presenta el mensaje. En el mensaje de presentación, puede ser el “Ordenante” o “el presentador”
   Write(FsTxt, '<InitgPty>');
       //Nombre de la parte
       WriteLn(FsTxt, '<Nm>'+CleanStr(FsNombrePresentador, INITIATOR_NAME_MAX_LENGTH)+'</Nm>');

       //Para el sistema de adeudos SEPA se utilizará exclusivamente la etiqueta “Otra” estructurada
       //según lo definido en el epígrafe “Identificador del presentador” de la sección 3.3
       WriteLn(FsTxt, '<Id>');
       WriteLn(FsTxt, '<OrgId>');
       WriteLn(FsTxt, '<Othr>');
       WriteLn(FsTxt, '<Id>'+FsIdPresentador+'</Id>');
       WriteLn(FsTxt, '</Othr>');
       WriteLn(FsTxt, '</OrgId>');
       WriteLn(FsTxt, '</Id>');
   Writeln(FsTxt,'</InitgPty>');

   Writeln(FsTxt, '</GrpHdr>');
end;


procedure TDJMNorma3414XML.CreateFile(AFileName :string);
var iOrdenante :Integer;
begin
   FsFileName := AFileName;
   AssignFile(FsTxt, AFileName);
   Rewrite(FsTxt);
   WriteLn(FsTxt, '<?xml version="1.0" encoding="UTF-8"?>');

   WriteLn(FsTxt,'<Document xmlns="urn:iso:std:iso:20022:tech:xsd:'+SCHEMA_34+'"'+
                     ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">');

   //MESSAGE ROOT. Identifica el tipo de mensaje
   WriteLn(FsTxt, '<CstmrCdtTrfInitn>'); //'<CstmrDrctDbtInitn>');
   WriteGroupHeader;
   //la info de cada Ordenante
   for iOrdenante := 1 to FiOrdenantes do begin
      if FListOrdenantes[iOrdenante].iPagos>0
      then writeOrdenesPago(FListOrdenantes[iOrdenante]);
   end;

   WriteLn(FsTxt, '</CstmrCdtTrfInitn>');
   WriteLn(FsTxt, '</Document>');
end;

procedure TDJMNorma3414XML.CloseFile;
begin
   Close(FsTxt);
end;

procedure TDJMNorma3414XML.writeOrdenesPago;
var iPago :Integer;
begin
   //2.0 Información del pago - PaymentInformation
   WriteLn(FsTxt, '<PmtInf>');
   //2.1 Identificación de Información del pago - PaymentInformationIdentification
   //Referencia asignada por el ordenante para identificar claramente el bloque de información de pago dentro del mensaje
   WriteLn(FsTxt, '<PmtInfId>'+CleanStr(oOrdenante.sPaymentId)+'</PmtInfId>');
   //2.2 Método de pago - PaymentMethod
   WriteLn(FsTxt, '<PmtMtd>'+'TRF'+'</PmtMtd>');
   //2.4 Número de operaciones - NumberOfTransactions
   WriteLn(FsTxt, '<NbOfTxs>'+IntToStr(oOrdenante.iPagos)+'</NbOfTxs>');
   //2.5 Con trol de suma - ControlSum
   //Suma total de todos los importes individuales incluidos en el bloque de información
   //de pago, sin tener en cuenta las divisas. Sirve como elemento de control.
   WriteLn(FsTxt, '<CtrlSum>'+FormatAmountXML(oOrdenante.mSumaImportes)+'</CtrlSum>');
   //2.6 Información del tipo de pago - PaymentTypeInformation
   WriteLn(FsTxt, '<PmtTpInf>');
   //2.8 Nivel de servicio - ServiceLevel
   WriteLn(FsTxt, '<SvcLvl><Cd>'+'SEPA'+'</Cd></SvcLvl>');
   WriteLn(FsTxt, '</PmtTpInf>');
   //2.17 Fecha de ejecución solicitada - Requested ExecutionDate
   WriteLn(FsTxt, '<ReqdExctnDt>'+FormatDateXML(FdOrdenesPago)+'</ReqdExctnDt>');
   //2.19 Ordenante - Debtor
   WriteLn(FsTxt, '<Dbtr><Nm>'+CleanStr(oOrdenante.sNombreOrdenante)+'</Nm></Dbtr>');

   //2.20 Cuenta del ordenante - DebtorAccount
   WriteLn(FsTxt, '<DbtrAcct>');
   WriteAccountIdentification(FsTxt, oOrdenante.sIBANOrdenante);
   WriteLn(FsTxt, '</DbtrAcct>');

   //2.21 Entidad del ordenante - DebtorAgent
   WriteLn(FsTxt, '<DbtrAgt>');
   WriteBICInfo(FsTxt, oOrdenante.sBICOrdenante);
   WriteLn(FsTxt, '</DbtrAgt>');

   //2.24 Cláusula de gastos - ChargeBearer
   //writeLn(FsTxt, '<ChrgBr>'+uSEPA_CleanString(ChrgBr)+'</ChrgBr>');

   for iPago := 1 to oOrdenante.iPagos do begin
      WriteCreditTransferOperationInfo(oOrdenante.ListPagos[iPago]);
   end;

   WriteLn(FsTxt, '</PmtInf>');
end;

procedure TDJMNorma3414XML.WriteCreditTransferOperationInfo;
begin
   //2.27 Información de tran sferencia individual - CreditTransferTran sactionInformation
   WriteLn(FsTxt, '<CdtTrfTxInf>');

   //2.28 Identificación del pago - PaymentIdentification
   Write(FsTxt, '<PmtId>');
   //2.30 Identificación de extremo a extremo - EndTo EndIdentification
   //Referencia única que asigna la parte i niciadora para identi ficar la operación
   //y que se transmite sin cambios a lo largo de la cadena del pago hasta el beneficiario.
   Write(FsTxt, '<EndToEndId>'+CleanStr(oPago.sIdPago)+'</EndToEndId>');
   WriteLn(FsTxt, '</PmtId>');

   //2.31 Información del tipo de pago – PaymentTypeInformation
   //<PmtTpInf>

   //2.42 Importe - Amoun t
   WriteLn(FsTxt, '<Amt><InstdAmt Ccy="'+'EUR'+'">'+FormatAmountXML(oPAgo.mImporte)+'</InstdAmt></Amt>');

   //2.77 Entidad del beneficiario - CreditorAgent
   WriteLn(FsTxt, '<CdtrAgt>');
   WriteBICInfo(FsTxt, oPago.sBICBeneficiario);
   WriteLn(FsTxt, '</CdtrAgt>');

   //2.79 Beneficiario - Creditor
   WriteLn(FsTxt, '<Cdtr><Nm>'+CleanStr(oPago.sNombreBeneficiario, BENEFICIARIO_NAME_MAX_LENGTH)+'</Nm></Cdtr>');

   //2.80 Cuenta del beneficiario - CreditorAccount
   WriteLn(FsTxt, '<CdtrAcct>');
   WriteAccountIdentification(FsTxt, oPago.sIBANBeneficiario);
   WriteLn(FsTxt, '</CdtrAcct>');

   //2.98 Concepto - RemittanceInformation
   WriteLn(FsTxt, '<RmtInf><Ustrd>'+CleanStr(oPago.sConcepto, RMTINF_MAX_LENGTH)+'</Ustrd></RmtInf>');

   WriteLn(FsTxt, '</CdtTrfTxInf>');
end;

procedure TDJMNorma3414XML.AddPago;
var iOrdenanteFound :Integer;
    iOrdenanteAux   :Integer;
    iPagosAux       :Integer;
begin
   //localizar en el arry de Ordenantes el iban, añadirlo en los pagos de ese Ordenante
   iOrdenanteFound := -1;
   for iOrdenanteAux := 1 to FiOrdenantes do begin
      if FListOrdenantes[iOrdenanteAux].sIBANOrdenante = sIBANOrdenante then begin
         iOrdenanteFound:=iOrdenanteAux;
      end;
   end;

   if iOrdenanteFound = -1 then begin
      ShowMessage('No se encontró Ordenante para el IBAN: '+sIBANOrdenante);
      Exit;
   end;

   if FListOrdenantes[iOrdenanteFound].iPagos = 5000 then begin
      ShowMessage('No admitimos más de 5000 pagos por Ordenante');
      Exit;
   end;

   //hemos encontrado el Ordenante con ese IBAN, añadimos un pago
   FListOrdenantes[iOrdenanteFound].iPagos := FListOrdenantes[iOrdenanteFound].iPagos+1;
   iPagosAux := FListOrdenantes[iOrdenanteFound].iPagos;

   FListOrdenantes[iOrdenanteFound].mSumaImportes := FListOrdenantes[iOrdenanteFound].mSumaImportes + mImporte;
   FmTotalImportes := FmTotalImportes + mImporte;

   FListOrdenantes[iOrdenanteFound].ListPagos[iPagosAux] := TInfoPago.Create;
   FListOrdenantes[iOrdenanteFound].ListPagos[iPagosAux].sIdPago             := sIdPago;
   FListOrdenantes[iOrdenanteFound].ListPagos[iPagosAux].mImporte            := mImporte;
   FListOrdenantes[iOrdenanteFound].ListPagos[iPagosAux].sBICBeneficiario    := sBICBeneficiario;
   FListOrdenantes[iOrdenanteFound].ListPagos[iPagosAux].sNombreBeneficiario := sNombreBeneficiario;
   FListOrdenantes[iOrdenanteFound].ListPagos[iPagosAux].sIBANBeneficiario   := sIBANBeneficiario;
   FListOrdenantes[iOrdenanteFound].ListPagos[iPagosAux].sConcepto           := sConcepto;
end;

procedure TDJMNorma3414XML.AddOrdenante;
var lFound :Boolean;
    iAux   :Integer;
begin
  if FiOrdenantes = 10 then begin
     ShowMessage('Solamente se admiten como máximo 10 Ordenantes');
     Exit;
  end;

  //si ya hay uno con esa cuenta, no lo añadimos
  lFound := False;
  for iAux := 1 to FiOrdenantes do begin
     if FListOrdenantes[iAux].sIBANOrdenante = sIBANOrdenante then lFound:=True;
  end;

  if not lFound then begin
     FiOrdenantes := FiOrdenantes+1;
     FListOrdenantes[FiOrdenantes]:= TInfoOrdenante.Create;
     FListOrdenantes[FiOrdenantes].mSumaImportes    := 0;
     FListOrdenantes[FiOrdenantes].sPayMentId       := sPayMentId;
     FListOrdenantes[FiOrdenantes].sNombreOrdenante := sNombreOrdenante;
     FListOrdenantes[FiOrdenantes].sIBANOrdenante   := sIBANOrdenante;
     FListOrdenantes[FiOrdenantes].sBICOrdenante    := sBICOrdenante;
     FListOrdenantes[FiOrdenantes].iPagos           := 0;
  end;
end;

function TDJMNorma3414XML.CalculateNumOperaciones;
var iOut           :Integer;
    iOrdenantesAux :Integer;
begin
   iOut := 0;
   for iOrdenantesAux := 1 to FiOrdenantes do begin
      iOut:=iOut+FListOrdenantes[iOrdenantesAux].iPagos;
   end;
   Result := iOut;
end;

function TDJMNorma3414XML.HayPagos;
begin
   Result := FmTotalImportes <> 0;
end;

procedure TDJMNorma3414XML.Clear;
var i :Integer;
    j :Integer;
begin
   for i := 1 to FiOrdenantes do begin
      //para cada Ordenante destruimos sus pagos
      for j := 1 to FlistOrdenantes[i].iPagos do begin
         FListOrdenantes[i].listPagos[j].free;
      end;

      FListOrdenantes[i].Free;
   end;
   FiOrdenantes    := 0;
   FmTotalImportes := 0;
end;

end.
