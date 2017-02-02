unit uDJMSepa1914XML;
{
https://github.com/cocosistemas/Delphi-SEPA-XML-ES
Diego J.Muñoz. Freelance. Cocosistemas.com
}
//2016-01-15
//ver los pdfs de los bancos, con la norma.
//19.14 cobros. EL Ordenante COBRA AL DEUDOR

//Tenemos un array de Ordenantes (**cada uno con un IBAN de abono**), y para cada Ordenante
//un array con sus ordenes de cobro

{uso:
   - setInfoPresentador
   - Añadimos Ordenantes: addOrdenante (uno por cada cuenta de ingreso del cobro, donde nos pagan)
   - Añadimos los cobros: addCobro (uno por cada cobro, él solo se coloca en su Ordenante,
     éste ha tenido que ser añadido previamente)
   - createfile (las ordenes estan en los arrays)
   - closefile
}

interface

uses senCille.CustomSEPA;

type
  //info de una orden de cobro (norma 19.14 xml)
  TInfoCobro = class
    sIdCobro         :string; //id unico cobro, ejemplo:20130930Fra.509301
    mImporte         :Double;
    sIdMandato       :string;
    dDateOfSignature :TDateTime; //del mandato
    sBIC             :string;
    sNombreDeudor    :string;
    sIBAN            :string;
    sConcepto        :string;
  end;

  TListOfCobros = array[1..5000] of TInfoCobro;

   //un conjunto de cobros por Ordenante, lo utilizamos por si utilizan
   //cobros a ingresar en diferentes cuentas (el <PmtInf> contiene la info del Ordenante, con su cuenta; y los
   //cobros relacionados con este Ordenante/cuenta de abono
   TInfoOrdenante = class
     sPayMentId       :string; //Ejemplo: 2013-10-28_095831Remesa 218 UNICO POR Ordenante
     mSumaImportes    :Double;
     sNombreOrdenante :string;
     sIBANOrdenante   :string;
     sBICOrdenante    :string;
     sIdOrdenante     :string; //el ID único del ordenante, normalmente dado por el banco
     listCobros       :TListOfCobros;
     iCobros          :Integer;
   end;

  TListOrdenantes = array[1..10] of TInfoOrdenante;

  TDJMNorma1914XML = class(TCustomSEPA) //el Ordenante cobra al DEUDOR
  private
    FsFileName      :string;
    FsTxt           :Text;
    FNumOrdenantes  :Integer;

    FListOrdenantes :TListOrdenantes; //Ordenantes, uno por cada cuenta de abono

    FdFileDate          :TDateTime;   //fecha del fichero

    FmTotalImportes     :Double;    //suma de los importes de los cobros
    FsNombrePresentador :string;    //nombre del presentador (el 'initiator')
    FsIdPresentador     :string;    //id presentador norma AT02
    FdOrdenesCobro      :TDateTime; //fecha del cargo en cuenta, PARA TODAS LAS ORDENES

    procedure WriteGroupHeader;
    procedure WriteOrdenesCobro(oOrdenante :TInfoOrdenante);
    procedure WriteDirectDebitOperationInfo(oCobro :TInfoCobro);

    procedure WriteInfoMandato(sIdMandato :string; dDateOfSignature :TDateTime);
    procedure WriteIdentificacionOrdenante(sIdOrdenanteAux :string);

    function CalculateNumOperaciones:Integer;
  public
    property NumOrdenantes  :Integer         read FNumOrdenantes;
    property ListOrdenantes :TListOrdenantes read FListOrdenantes;
    constructor Create;
    destructor Destroy; reintroduce;
    procedure SetInfoPresentador(dFileDate          :TDateTime;
                                 sNombrePresentador :string;
                                 sIdPresentador     :string;
                                 dOrdenesCobro      :TDateTime);
    procedure AddOrdenante(sPayMentId       :string;
                           sNombreOrdenante :string;
                           sIBANOrdenante   :string;
                           sBICOrdenante    :string;
                           sIdOrdenante     :string);

    procedure AddCobro(sIdCobro         :string; //id unico cobro, ejemplo:20130930Fra.509301
                       mImporte         :Double;
                       sIdMandato       :string;
                       dDateOfSignature :TDateTime; //del mandato
                       sBIC             :string;
                       sNombreDeudor    :string;
                       sIBAN            :string;
                       sConcepto        :string;
                       sIBANOrdenante   :string); //el cobro lo colocamos en la info de su Ordenante, por la cuenta
    procedure CreateFile(sFileName :string);
    procedure CloseFile;
    function HayCobros:Boolean;
  end;

implementation
uses System.SysUtils, Dialogs;

constructor TDJMNorma1914XML.Create;
begin
   FNumOrdenantes      := 0;
   FdFileDate          := Now;
   FmTotalImportes     := 0;
   FsNombrePresentador := '';
   FsIdPresentador     := '';
   FdOrdenesCobro      := Now;




end;

procedure TDJMNorma1914XML.SetInfoPresentador;
begin
   FdFileDate          := dFileDate;
   FsNombrePresentador := sNombrePresentador;
   FsIdPresentador     := sIdPresentador;
   FdOrdenesCobro      := dOrdenesCobro;
   //FsPaymentId:=sPaymentId;

   //FsNombreOrdenante:=sNombreOrdenante;
   //FsIBANOrdenante:=sIBANOrdenante;
   //FsBICOrdenante:=sBICOrdenante;
end;

destructor TDJMNorma1914XML.Destroy;
var i :Integer;
    j :Integer;
begin
   for i := 1 to FNumOrdenantes do begin
      //para cada Ordenante destruimos sus cobros
      for j := 1 to FlistOrdenantes[i].iCobros do begin
         FListOrdenantes[i].ListCobros[j].Free;
      end;

      FListOrdenantes[i].Free;
   end;

   inherited Destroy;
end;

procedure TDJMNorma1914XML.WriteGroupHeader;
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


procedure TDJMNorma1914XML.CreateFile;
var iOrdenante :Integer;
begin
   FsFileName:=sFileName;
   assignFile(FsTxt,sFileName);
   rewrite(FsTxt);
   WriteLn(FsTxt, '<?xml version="1.0" encoding="UTF-8"?>');

   WriteLn(FsTxt,
   '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:'+SCHEMA_19+'"'+
                     ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">');

   //MESSAGE ROOT. Identifica el tipo de mensaje: iniciación de adeudos directos
   WriteLn(FsTxt, '<CstmrDrctDbtInitn>');
   writeGroupHeader;
   //la info de cada Ordenante
   for iOrdenante := 1 to FNumOrdenantes do begin
      if FListOrdenantes[iOrdenante].iCobros > 0 then writeOrdenesCobro(FListOrdenantes[iOrdenante]);
   end;

   WriteLn(FsTxt, '</CstmrDrctDbtInitn>');
   WriteLn(FsTxt, '</Document>'         );
end;

procedure TDJMNorma1914XML.CloseFile;
begin
   Close(FsTxt);
end;

procedure TDJMNorma1914XML.writeOrdenesCobro;
var iCobro :Integer;
begin
   //2.0 1..n Conjunto de características que se aplican a la parte del Ordenante de
   //las operaciones de pago incluidas en el mensaje de iniciación de adeudos directos
   WriteLn(FsTxt, '<PmtInf>');

   //2.1 Referencia única, asignada por el presentador, para identificar inequívocamente
   //el bloque de información del pago dentro del mensaje
   WriteLn(FsTxt, '<PmtInfId>'+CleanStr(oOrdenante.sPaymentId)+'</PmtInfId>');

   //2.2 Especifica el medio de pago que se utiliza para mover los fondos.
   //Fijo a DD
   WriteLn(FsTxt, '<PmtMtd>'+'DD'+'</PmtMtd>');

   //2.3 <BtchBookg> Info de apunte en cuenta, no lo ponemos

   //2.4 <NbOfTxs> Nº DE OPERACIONES, NO LO PONEMOS
   //writeLn(FsTxt, '<NbOfTxs>'+IntToStr(NbOfTxs)+'</NbOfTxs>');

   //2.5 Suma total de todos los importes individuales incluidos en el bloque ‘Información del pago’,
   //sin tener en cuenta la divisa de los importes. No lo ponemos
   //writeLn(FsTxt, '<CtrlSum>'+SEPAFormatAmount(oOrdenante.mSumaImportes)+'</CtrlSum>');

   //2.6 Información del tipo de pago
   WriteLn(FsTxt, '<PmtTpInf>');

   //2.8 Nivel de servicio
   WriteLn(FsTxt, '<SvcLvl>');
   //2.9 Código del nivel de servicio, fijo a 'SEPA'
   WriteLn(FsTxt, '<Cd>'+'SEPA'+'</Cd>');
   Writeln(FsTxt, '</SvcLvl>');

   //2.10 NO HAY

   //2.11 Instrumento específico del esquema SEPA
   Write(FsTxt, '<LclInstrm>');

   //2.12  Esquema bajo cuyas reglas ha de procesarse la operación (AT-20), fijo a 'CORE'
   WriteLn(FsTxt, '<Cd>'+'CORE'+'</Cd>');
   WriteLn(FsTxt, '</LclInstrm>');

   //2.14  Secuencia del adeudo. Los dejamos todos en RCUR
   writeLn(FsTxt, '<SeqTp>'+'RCUR'+'</SeqTp>');

   WriteLn(FsTxt, '</PmtTpInf>');

   //2.18 Fecha de cobro: RequestedCollectionDate
   //Fecha solicitada por el Ordenante para realizar el cargo en la cuenta del deudor (AT-11)
   WriteLn(FsTxt, '<ReqdColltnDt>'+FormatDateXML(FdOrdenesCobro)+'</ReqdColltnDt>');

   //2.19 Ordenante – Creditor
   WriteLn(FsTxt, '<Cdtr><Nm>'+CleanStr(oOrdenante.sNombreOrdenante, ORDENANTE_NAME_MAX_LENGTH)+'</Nm></Cdtr>');

   //2.20 Cuenta del Ordenante – CreditorAccount
   //Identificación inequívoca de la cuenta del Ordenante (AT-04)
   WriteLn(FsTxt, '<CdtrAcct>');
   WriteAccountIdentification(FsTxt, oOrdenante.sIBANOrdenante);
   WriteLn(FsTxt, '</CdtrAcct>');

   //2.21 Entidad del Ordenante – CreditorAgent
   //Entidad de crédito donde el Ordenante mantiene su cuenta.
   WriteLn(FsTxt, '<CdtrAgt>');
   WriteBICInfo(FsTxt, oOrdenante.sBICOrdenante);
   WriteLn(FsTxt, '</CdtrAgt>');

   //2.24 Cláusula de gastos – ChargeBearer
   //Especifica qué parte(s) correrá(n) con los costes asociados al tratamiento de la operación de pago
   //Fijo a 'SLEV'
   WriteLn(FsTxt, '<ChrgBr>'+'SLEV'+'</ChrgBr>');


   //2.27 Identificación del Ordenante – CreditorSchemeIdentification
   WriteIdentificacionOrdenante(oOrdenante.sIdOrdenante);

   //2.28 1..n Información de la operación de adeudo directo – DirectDebitTransactionInformation
   for iCobro := 1 to oOrdenante.iCobros do begin
      //DrctDbtTxInfEntry[i].SaveToStream(FsTxt, schema);
      writeDirectDebitOperationInfo(oOrdenante.ListCobros[iCobro]);
   end;

   WriteLn(FsTxt, '</PmtInf>');
end;

procedure TDJMNorma1914XML.writeDirectDebitOperationInfo;
begin
   //2.28 1..n Información de la operación de adeudo directo – DirectDebitTransactionInformation
   WriteLn(FsTxt,  '<DrctDbtTxInf>');

   //2.29 Identificación del pago – PaymentIdentification
   WriteLn(FsTxt, '<PmtId>');
   //2.31 Identificación de extremo a extremo – EndToEndIdentification
   //Identificación única asignada por la parte iniciadora para identificar inequívocamente
   //cada operación (AT-10). Esta referencia se transmite de extremo a extremo,
   //sin cambios, a lo largo de toda la cadena de pago
   Writeln(FsTxt, '<EndToEndId>'+CleanStr(oCobro.sIdCobro)+'</EndToEndId>');
   Writeln(FsTxt, '</PmtId>');

   //2.44 Importe ordenado – InstructedAmount
   WriteLn(FsTxt,  '<InstdAmt Ccy="'+'EUR'+'">'+FormatAmountXML(oCobro.mImporte)+'</InstdAmt>');

   //2.46 Operación de adeudo directo – DirectDebitTransaction
   //Conjunto de elementos que suministran información específica relativa al mandato de adeudo directo
   WriteLn(FsTxt,  '<DrctDbtTx>');
   WriteInfoMandato(oCobro.sIdMandato,oCobro.dDateOfSignature);
   WriteLn(FsTxt,  '</DrctDbtTx>');

   //2.66 Identificación del Ordenante – CreditorSchemeIdentification
   //es como el 2.27. No lo ponemos porque ya ponemos el 2.27
   //writeIdentificacionOrdenante(sIdOrdenanteAux);

   //2.70 Entidad del deudor – DebtorAgent
   WriteLn(FsTxt,  '<DbtrAgt>');
   WriteBICInfo(FsTxt, oCobro.sBIC);
   WriteLn(FsTxt,  '</DbtrAgt>');

   //2.72 Deudor – Debtor
   WriteLn(FsTxt,  '<Dbtr><Nm>'+CleanStr(oCobro.sNombreDeudor, DEUDOR_NAME_MAX_LENGTH)+'</Nm></Dbtr>');

   //2.73 Cuenta del deudor – DebtorAccount
   WriteLn(FsTxt,  '<DbtrAcct>');
   WriteAccountIdentification(FsTxt, oCobro.sIBAN);
   WriteLn(FsTxt,  '</DbtrAcct>');

   {
   if UltmtDbtrNm <> '' then
     //2.74 Último deudor – UltimateDebtor
     WriteLn(FsTxt,  '<UltmtDbtr><Nm>'+uSEPA_CleanStr(UltmtDbtrNm, DBTR_NM_MAX_LEN)+'</Nm></UltmtDbtr>');
   }

   //2.88 Concepto – RemittanceInformation
   //Información que opcionalmente remite el Ordenante al deudor para permitirle conciliar el pago
   //con la información comercial del mismo (AT-22).
   WriteLn(FsTxt,  '<RmtInf><Ustrd>'+CleanStr(oCobro.sConcepto, RMTINF_MAX_LENGTH)+'</Ustrd></RmtInf>');

   WriteLn(FsTxt,  '</DrctDbtTxInf>');
end;

procedure TDJMNorma1914XML.writeInfoMandato;
begin
   //2.47 Información del mandato – MandateRelatedInformation
   WriteLn(FsTxt, '<MndtRltdInf>');
   //2.48 Identificación del mandato – MandateIdentification.
   //Por ejemplo un nº o algo así
   WriteLn(FsTxt, '<MndtId>'+CleanStr(sIdMandato, MNDTID_MAX_LENGTH)+'</MndtId>');
   //2.49 Fecha de firma – DateOfSignature
   WriteLn(FsTxt, '<DtOfSgntr>'+FormatDateXML(dDateOfSignature)+'</DtOfSgntr>');
   //2.50 Indicador de modificación – AmendmentIndicator
   WriteLn(FsTxt, '<AmdmntInd>'+'false'+'</AmdmntInd>');
   {
   if AmdmntInd 'es True' then
     //escribir la info completa de la etiqueta <AmdmntInfDtls>
   }
   WriteLn(FsTxt, '</MndtRltdInf>');
end;

procedure TDJMNorma1914XML.addCobro;
var iOrdenanteFound :Integer;
    iOrdenanteAux   :Integer;
    iCobrosAux      :Integer;
begin
   //localizar en el array de Ordenantes el iban, añadirlo en los cobros de ese Ordenante
   iOrdenanteFound := -1;
   for iOrdenanteAux := 1 to FNumOrdenantes do begin
      if FListOrdenantes[iOrdenanteAux].sIBANOrdenante = sIBANOrdenante then begin
         iOrdenanteFound:=iOrdenanteAux;
      end;
   end;

   if iOrdenanteFound = -1 then begin
      ShowMessage('No se encontró Ordenante para el IBAN: '+sIBANOrdenante);
      Exit;
   end;

   if FListOrdenantes[iOrdenanteFound].iCobros = 5000 then begin
      ShowMessage('No admitimos más de 5000 cobros por Ordenante');
      Exit;
   end;

   //hemos encontrado el Ordenante con ese IBAN, añadimos un cobro
   FListOrdenantes[iOrdenanteFound].iCobros:= FListOrdenantes[iOrdenanteFound].iCobros+1;
   iCobrosAux                              := FListOrdenantes[iOrdenanteFound].iCobros;

   FListOrdenantes[iOrdenanteFound].mSumaImportes := FListOrdenantes[iOrdenanteFound].mSumaImportes+mImporte;
   FmTotalImportes                                := FmTotalImportes + mImporte;

   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux] := TInfoCobro.Create;
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].sIdCobro         := sIdCobro;
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].mImporte         := mImporte;
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].sIdMandato       := sIdMandato;
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].dDateOfSignature := dDateOfSignature;
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].sBIC             := sBIC;
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].sNombreDeudor    := sNombreDeudor;
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].sIBAN            := Trim(sIBAN);
   FListOrdenantes[iOrdenanteFound].ListCobros[iCobrosAux].sConcepto        := sConcepto;
end;

procedure TDJMNorma1914XML.AddOrdenante;
var lFound :Boolean;
    iAux   :Integer;
begin
   if FNumOrdenantes = 10 then begin
      ShowMessage('Solamente se admiten como máximo 10 Ordenantes');
      Exit;
   end;

   //si ya hay uno con esa cuenta, no lo añadimos
   lFound := False;
   for iAux := 1 to FNumOrdenantes do begin
      if FListOrdenantes[iAux].sIBANOrdenante = sIBANOrdenante then lFound := True;
   end;

   if not lFound then begin
      FNumOrdenantes := FNumOrdenantes + 1;
      FListOrdenantes[FNumOrdenantes]:= TInfoOrdenante.Create;
      FListOrdenantes[FNumOrdenantes].mSumaImportes    := 0;
      FListOrdenantes[FNumOrdenantes].sPayMentId       := sPayMentId;
      FListOrdenantes[FNumOrdenantes].sNombreOrdenante := sNombreOrdenante;
      FListOrdenantes[FNumOrdenantes].sIBANOrdenante   := sIBANOrdenante;
      FListOrdenantes[FNumOrdenantes].sBICOrdenante    := sBICOrdenante;
      FListOrdenantes[FNumOrdenantes].sIdOrdenante     := sIdOrdenante;
      FListOrdenantes[FNumOrdenantes].iCobros          := 0;
   end;
end;

function TDJMNorma1914XML.CalculateNumOperaciones;
var iOut           :Integer;
    iOrdenantesAux :Integer;
begin
   iOut := 0;
   for iOrdenantesAux := 1 to FNumOrdenantes do begin
       iOut := iOut + FListOrdenantes[iOrdenantesAux].iCobros;
   end;

   Result := iOut;
end;

function TDJMNorma1914XML.HayCobros;
begin
   Result := FmTotalImportes <> 0;
end;

procedure TDJMNorma1914XML.writeIdentificacionOrdenante;
begin
   WriteLn(FsTxt, '<CdtrSchmeId>');
   WriteLn(FsTxt, '<Id>'         );
   WriteLn(FsTxt, '<PrvtId>'     );
   WriteLn(FsTxt, '<Othr>'       );
   WriteLn(FsTxt, '<Id>' + CleanStr(sIdOrdenanteAux) + '</Id>');
   WriteLn(FsTxt, '<SchmeNm><Prtry>SEPA</Prtry></SchmeNm>');
   WriteLn(FsTxt, '</Othr>'       );
   WriteLn(FsTxt, '</PrvtId>'     );
   WriteLn(FsTxt, '</Id>'         );
   writeLn(FsTxt, '</CdtrSchmeId>');
end;

end.
