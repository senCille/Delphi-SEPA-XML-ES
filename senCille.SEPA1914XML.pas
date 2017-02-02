unit senCille.SEPA1914XML;
    {https://github.com/sencille/Delphi-SEPA-XML-ES
    Juan C.Cilleruelo Gonzalo. senCille.es
    Based on a previous version donated by:
          https://github.com/cocosistemas/Delphi-SEPA-XML-ES
          Diego J.Muñoz. Freelance. Cocosistemas.com         }

{ 2017-02-02
  ver los pdfs de los bancos, con la norma.
  19.14 cobros. El Initiator COBRA AL DEUDOR }

 {Tenemos una Lista de Ordenantes (**cada uno con un IBAN de abono**), y en cada Ordenante
  una lista con sus operaciones de cobro

{Uso: Creamos la estructura de clases de manera externa.
   Primero creamos el Initiator y a continuación añadidos cuantas oparaciones sean
   necesarias en su propiedad Operations.

   Finalmente creamos el objeto de esta unidad.
   Asignamos valor a las propiedades necesarias (5 al menos).
   Asignamos el Iniciator a su propiedad Initiator.
   Ejecutamos la instrucción SaveToFile:
}

interface

uses System.Generics.Collections,
     senCille.SEPAAuxClasses,
     senCille.CustomSEPA;

type
  TNorma1914xml = class(TCustomSEPA) {el Ordenante cobra al DEUDOR}
  private
    FInitiators :TList<TsepaInitiator>; {Ordenantes, uno por cada cuenta de abono}

    procedure AddGroupHeader;
    procedure AddOperations   (AInitiator   :TsepaInitiator);
    procedure AddDirectDebitOp(AOperation   :TsepaOperation);
    procedure AddInfoMandator (AIdMandator  :string; ADateOfSignature :TDateTime);
    procedure AddIdInitiator  (AIdInitiator :string);

    function GetNumOperations:Integer;
    function GetTotalImport  :Double;
  public
    constructor Create;
    destructor Destroy; reintroduce;
    procedure SaveToFile;
    function ThereAreOperations:Boolean;
    property Initiators :TList<TsepaInitiator> read FInitiators;
  end;

implementation

uses System.Classes, System.SysUtils, Dialogs;

constructor TNorma1914xml.Create;
begin
   inherited;
   FInitiators := TList<TsepaInitiator>.Create; {Ordenantes, uno por cada cuenta de abono}
end;

destructor TNorma1914xml.Destroy;
begin
   FInitiators.Free;
   inherited Destroy;
end;

procedure TNorma1914xml.AddGroupHeader;
begin
   {1.0 Group Header Conjunto de características compartidas por todas las operaciones incluidas en el mensaje}
   AddLine('<GrpHdr>');

   {1.1 MessageId Referencia asignada por la parte iniciadora y enviada a la siguiente }
   {    parte de la cadena para identificar el mensaje de forma inequívoca             }
   AddLine('   <MsgId>'+CleanStr(GenerateUUID)+'</MsgId>');

   {1.2 Fecha y hora cuando la parte iniciadora ha creado un (grupo de) instrucciones de pago }
   {  (con 'now' es suficiente)                                                               }
   AddLine(   '<CreDtTm>'+FormatDateTimeXML(FileDate)+'</CreDtTm>');

   {1.6  Número de operaciones individuales que contiene el mensaje }
   AddLine('   <NbOfTxs>'+IntToStr(GetNumOperations)+'</NbOfTxs>');

   {1.7 Suma total de todos los importes individuales incluidos en el mensaje}
   AddLine('   <CtrlSum>'+FormatAmountXML(GetTotalImport)+'</CtrlSum>');

   {1.8 Parte que presenta el mensaje. En el mensaje de presentación, puede ser el “Ordenante” o “el presentador”}
   AddLine('<InitgPty>');
       {Nombre de la parte}
       AddLine('<Nm>'+CleanStr(InitiatorName, INITIATOR_NAME_MAX_LENGTH)+'</Nm>');
       {Para el sistema de adeudos SEPA se utilizará exclusivamente la etiqueta “Otra” estructurada  }
       {  según lo definido en el epígrafe “Identificador del presentador” de la sección 3.3         }
       AddLine('   <Id>'                    );
       AddLine('   <OrgId>'                 );
       AddLine('   <Othr>'                  );
       AddLine('   <Id>'+InitiatorId+'</Id>');
       AddLine('   </Othr>'                 );
       AddLine('   </OrgId>'                );
       AddLine('   </Id>'                   );
   AddLine('</InitgPty>');

   AddLine('</GrpHdr>');
end;


procedure TNorma1914xml.SaveToFile;
var Initiator :TsepaInitiator;
begin
   FOutput := TStringList.Create;
   try
      AddLine('<?xml version="1.0" encoding="UTF-8"?>');
      AddLine('<Document xmlns="urn:iso:std:iso:20022:tech:xsd:'+SCHEMA_19+'"'+' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">');

      { MESSAGE ROOT. Identifica el tipo de mensaje: iniciación de adeudos directos }
      AddLine('<CstmrDrctDbtInitn>');
      AddGroupHeader;
      { la info de cada Ordenante }
      for Initiator in FInitiators do begin
         if Initiator.Operations.Count > 0 then AddOperations(Initiator);
      end;

      AddLine('</CstmrDrctDbtInitn>');
      AddLine('</Document>'         );
      FOutput.SaveToFile(FileName);
   finally
      FOutput.Free;
   end;
end;

procedure TNorma1914xml.AddOperations(AInitiator :TsepaInitiator);
var Operation :TsepaOperation;
begin
   { 2.0 1..n Conjunto de características que se aplican a la parte del Ordenante de    }
   { las operaciones de pago incluidas en el mensaje de iniciación de adeudos directos  }
   AddLine('<PmtInf>');

   { 2.1 Referencia única, asignada por el presentador, para identificar inequívocamente  }
   { el bloque de información del pago dentro del mensaje                                 }
   AddLine('<PmtInfId>'+CleanStr(AInitiator.PaymentId)+'</PmtInfId>');

   { 2.2 Especifica el medio de pago que se utiliza para mover los fondos.  }
   { Fijo a DD                                                              }
   AddLine('<PmtMtd>'+'DD'+'</PmtMtd>');

   { 2.3 <BtchBookg> Info de apunte en cuenta, no lo ponemos }

   { 2.4 <NbOfTxs> Nº DE OPERACIONES, NO LO PONEMOS }
   //writeLn(FOuputFile, '<NbOfTxs>'+IntToStr(NbOfTxs)+'</NbOfTxs>');

   { 2.5 Suma total de todos los importes individuales incluidos en el bloque ‘Información del pago’, }
   {   sin tener en cuenta la divisa de los importes. No lo ponemos                                   }
   //writeLn(FOuputFile, '<CtrlSum>'+SEPAFormatAmount(Initiators.mSumaImportes)+'</CtrlSum>');

   { 2.6 Información del tipo de pago }
   AddLine('<PmtTpInf>');

   { 2.8 Nivel de servicio }
   AddLine('<SvcLvl>');
   { 2.9 Código del nivel de servicio, fijo a 'SEPA' }
   AddLine('<Cd>'+'SEPA'+'</Cd>');
   AddLine('</SvcLvl>');

   { 2.10 NO HAY }

   { 2.11 Instrumento específico del esquema SEPA }
   AddLine('<LclInstrm>');

   { 2.12  Esquema bajo cuyas reglas ha de procesarse la operación (AT-20), fijo a 'CORE' }
   AddLine('<Cd>'+'CORE'+'</Cd>');
   AddLine('</LclInstrm>');

   { 2.14  Secuencia del adeudo. Los dejamos todos en RCUR }
   AddLine('<SeqTp>'+'RCUR'+'</SeqTp>');

   AddLine('</PmtTpInf>');

   { 2.18 Fecha de cobro: RequestedCollectionDate                                              }
   { Fecha solicitada por el Ordenante para realizar el cargo en la cuenta del deudor (AT-11)  }
   AddLine('<ReqdColltnDt>'+FormatDateXML(ChargeDate)+'</ReqdColltnDt>');

   { 2.19 Ordenante – Creditor }
   AddLine('<Cdtr><Nm>'+CleanStr(AInitiator.Name, ORDENANTE_NAME_MAX_LENGTH)+'</Nm></Cdtr>');

   { 2.20 Cuenta del Ordenante – CreditorAccount                  }
   { Identificación unívoca de la cuenta del Ordenante (AT-04)    }
   AddLine('<CdtrAcct>');
   AddAccountId(AInitiator.IBAN);
   AddLine('</CdtrAcct>');

   { 2.21 Entidad del Ordenante – CreditorAgent                }
   { Entidad de crédito donde el Ordenante mantiene su cuenta. }
   AddLine('<CdtrAgt>');
   AddBICInfo(AInitiator.BIC);
   AddLine('</CdtrAgt>');

   { 2.24 Cláusula de gastos – ChargeBearer                                                              }
   { Especifica qué parte(s) correrá(n) con los costes asociados al tratamiento de la operación de pago  }
   { Fijo a 'SLEV'                                                                                       }
   AddLine('<ChrgBr>'+'SLEV'+'</ChrgBr>');


   { 2.27 Identificación del Ordenante – CreditorSchemeIdentification }
   AddIdInitiator(AInitiator.IdInitiator);

   { 2.28 1..n Información de la operación de adeudo directo – DirectDebitTransactionInformation }
   for Operation in AInitiator.Operations do begin
      AddDirectDebitOp(Operation);
   end;

   AddLine('</PmtInf>');
end;

procedure TNorma1914xml.AddDirectDebitOp(AOperation :TsepaOperation);
begin
   { 2.28 1..n Información de la operación de adeudo directo – DirectDebitTransactionInformation }
   AddLine('<DrctDbtTxInf>');

   { 2.29 Identificación del pago – PaymentIdentification }
   AddLine('<PmtId>');
   { 2.31 Identificación de extremo a extremo – EndToEndIdentification                       }
   { Identificación única asignada por la parte iniciadora para identificar inequívocamente  }
   { cada operación (AT-10). Esta referencia se transmite de extremo a extremo,              }
   { sin cambios, a lo largo de toda la cadena de pago                                       }
   AddLine('<EndToEndId>'+CleanStr(AOperation.OpId)+'</EndToEndId>');
   AddLine('</PmtId>');

   { 2.44 Importe ordenado – InstructedAmount }
   AddLine('<InstdAmt Ccy="'+'EUR'+'">'+FormatAmountXML(AOperation.Import)+'</InstdAmt>');

   { 2.46 Operación de adeudo directo – DirectDebitTransaction                                          }
   { Conjunto de elementos que suministran información específica relativa al mandato de adeudo directo }
   AddLine('<DrctDbtTx>');
   AddInfoMandator(AOperation.IdMandator, AOperation.DateOfSignature);
   AddLine('</DrctDbtTx>');

   { 2.66 Identificación del Ordenante – CreditorSchemeIdentification }
   { es como el 2.27. No lo ponemos porque ya ponemos el 2.27         }
   //WriteIdInitiator(AIdOrdenanteAux);

   { 2.70 Entidad del deudor – DebtorAgent }
   AddLine('<DbtrAgt>');
   AddBICInfo(AOperation.BIC);
   AddLine('</DbtrAgt>');

   { 2.72 Deudor – Debtor }
   AddLine('<Dbtr><Nm>'+CleanStr(AOperation.Name, DEUDOR_NAME_MAX_LENGTH)+'</Nm></Dbtr>');

   { 2.73 Cuenta del deudor – DebtorAccount }
   AddLine('<DbtrAcct>');
   AddAccountId(AOperation.IBAN);
   AddLine('</DbtrAcct>');

   {
   if UltmtDbtrNm <> '' then
     { 2.74 Último deudor – UltimateDebtor
     WriteLn(FOuputFile,  '<UltmtDbtr><Nm>'+uSEPA_CleanStr(UltmtDbtrNm, DBTR_NM_MAX_LEN)+'</Nm></UltmtDbtr>');
   }

   { 2.88 Concepto – RemittanceInformation                                                          }
   { Información que opcionalmente remite el Ordenante al deudor para permitirle conciliar el pago  }
   { con la información comercial del mismo (AT-22).                                                }
   AddLine('<RmtInf><Ustrd>'+CleanStr(AOperation.Concept, RMTINF_MAX_LENGTH)+'</Ustrd></RmtInf>');

   AddLine('</DrctDbtTxInf>');
end;

procedure TNorma1914xml.AddInfoMandator(AIdMandator  :string; ADateOfSignature :TDateTime);
begin
   { 2.47 Información del mandato – MandateRelatedInformation }
   AddLine('<MndtRltdInf>');
   { 2.48 Identificación del mandato – MandateIdentification.  }
   { Por ejemplo un nº o algo así                              }
   AddLine('<MndtId>'+CleanStr(AIdMandator, MNDTID_MAX_LENGTH)+'</MndtId>');
   { 2.49 Fecha de firma – DateOfSignature }
   AddLine('<DtOfSgntr>'+FormatDateXML(ADateOfSignature)+'</DtOfSgntr>');
   { 2.50 Indicador de modificación – AmendmentIndicator }
   AddLine('<AmdmntInd>'+'false'+'</AmdmntInd>');
   {
   if AmdmntInd 'es True' then
     //escribir la info completa de la etiqueta <AmdmntInfDtls>
   }
   AddLine('</MndtRltdInf>');
end;

function TNorma1914xml.GetNumOperations:Integer;
var i :TsepaInitiator;
begin
   Result := 0;
   for i in FInitiators do begin
      Result := Result + i.Operations.Count;
   end;
end;

function TNorma1914xml.GetTotalImport:Double;
var i :TsepaInitiator;
begin
   Result := 0;
   for i in FInitiators do begin
      Result := Result + i.GetTotalImport;
   end;
end;

function TNorma1914xml.ThereAreOperations;
begin
   Result := GetTotalImport <> 0;
end;

procedure TNorma1914xml.AddIdInitiator(AIdInitiator :string);
begin
   AddLine('<CdtrSchmeId>');
   AddLine('<Id>'         );
   AddLine('<PrvtId>'     );
   AddLine('<Othr>'       );
   AddLine('<Id>' + CleanStr(AIdInitiator) + '</Id>');
   AddLine('<SchmeNm><Prtry>SEPA</Prtry></SchmeNm>');
   AddLine('</Othr>'       );
   AddLine('</PrvtId>'     );
   AddLine('</Id>'         );
   AddLine('</CdtrSchmeId>');
end;

end.
