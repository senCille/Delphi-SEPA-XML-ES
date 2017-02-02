unit fMain;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TfrMain = class(TForm)
    btnTestNorma19: TButton;
    btnTestNorma34: TButton;
    procedure btnTestNorma19Click(Sender: TObject);
    procedure btnTestNorma34Click(Sender: TObject);
  end;

var frMain :TfrMain;

implementation

uses senCille.SEPAAuxClasses,
     uDJMSepa1914XML,
     uDJMSepa3414XML;

{$R *.dfm}

procedure TfrMain.btnTestNorma19Click(Sender: TObject);
var Norma1914 :TDJMNorma1914XML;
    Initiator :TsepaInitiator;
    Operation :TsepaOperation;
begin
   {Un Ordenante. Un presentador puede presentar las órdenes de varios ordenantes}
   Initiator := TsepaInitiator.Create;
   Initiator.IdInitiator := 'ID UNICO DE LAS ORDENES DE ESTE ORDENANTE';
   Initiator.Name        := 'EMPRESA ORDENANTE 1 S.L.'                 ;
   Initiator.IBAN        := 'el iban de este ordenante'                ;
   Initiator.BIC         := 'BicOrdenante1'                            ;
   {----------------------------------------------------------------------------------}
   Initiator.IdInitiator := 'ID.ORDENANTE'; {el ID único del ordenante, normalmente dado por el banco}

   //las ordenes de cobro de este ordenante
   Operation := TsepaOperation.Create;
   Operation.OpId            := 'idCobro unico'; {utilizar un nº de documento o contador, etc}
   Operation.Import          := 1200;
   Operation.IdMandator      := 'id.mandato'; {por ejemplo, poner un nº de contrato o del documento de la firma del mandato}
   Operation.DateOfSignature := StrToDate('31/10/2009'); {esta es la fecha por defecto de los que no tienen fecha de mandato}
   Operation.BIC             := 'Bic de este Deudor';
   Operation.Name            := 'Nombre Deudor 1 S.L.';
   Operation.IBAN            := 'IBAN De este Deudor';
   Operation.Concept         := 'Cobro de pruebas factura nº 1';
   Initiator.Operations.Add(Operation);

   {Segunda Orden de Cobro}
   Operation := TsepaOperation.Create;
   Operation.OpId            := 'idCobro unico-2'              ; {utilizar un nº de documento o contador, etc}
   Operation.Import          := 230                            ;
   Operation.IdMandator      := 'id.mandato'                   ;
   Operation.DateOfSignature := StrToDate('31/10/2009')        ; //esta es la fecha por defecto de los que no tienen fecha de mandato
   Operation.BIC             := 'Bic de este Deudor'           ;
   Operation.Name            := 'Nombre Deudor 2 S.L.'         ;
   Operation.IBAN            := 'IBAN De este Deudor'          ;
   Operation.Concept         := 'Cobro de pruebas factura nº 1';
   Initiator.Operations.Add(Operation);

   Norma1914 := TDJMNorma1914XML.Create;
   try
      {Información general del fichero}
      {El id. del identificador te lo dará el Banco, LEER LAS NORMAS SEPA!!}
      Norma1914.FileDate      := Date;
      Norma1914.InitiatorName := 'NOMBRE DEL PRESENTADOR S.L.';
      Norma1914.InitiatorId   := 'ID.PRESENTADOR';
      Norma1914.ChargeDate    := Date + 2;

      Norma1914.AddInitiator(Initiator);

      if Norma1914.ThereAreOperations then begin
         Norma1914.CreateFile('test-1914.xml');
         Norma1914.CloseFile;
         ShowMessage('Fichero 1914 creado');
      end
      else ShowMessage('No hay cobros');
   finally
      Norma1914.Free;
   end;
end;

procedure TfrMain.btnTestNorma34Click(Sender: TObject);
var Norma3414 :TDJMNorma3414XML;
    Initiator :TsepaInitiator;
    Operation :TsepaOperation;
begin
   Initiator := TsepaInitiator.Create;
   Initiator.IdInitiator := 'ID. UNICO DEL PAGO'       ;
   Initiator.Name        := 'NOMBRE DEL ORDENANTE S.L.';
   Initiator.IBAN        := 'IBAN DEL ORDENANTE'       ;
   Initiator.BIC         := 'BIC DEL ORDENANTE'        ;
   {----------------------------------------------------------------------------------}
   Initiator.IdInitiator := 'ID.ORDENANTE'; {el ID único del ordenante, normalmente dado por el banco}

   Operation := TsepaOperation.Create;
   Operation.OpId    := 'ID UNICO DEL PAGO'             ; {utilizar un nº de documento o contador, etc}
   Operation.Import  := 500                             ;
   Operation.BIC     := 'BIC DEL BENEFICIARIO'          ;
   Operation.Name    := 'NOMBRE DEL BENEFICIARIO 1 S.L.';
   Operation.IBAN    := 'IBAN DEL BENEFICIARIO'         ;
   Operation.Concept := 'Pago de su factura nº 5698'    ;
   Initiator.Operations.Add(Operation);

   Norma3414 := TDJMNorma3414XML.create;
   try
      {info del presentador}
      Norma3414.FileDate      := Date;
      Norma3414.InitiatorName := 'NOMBRE DEL PRESENTADOR';
      Norma3414.InitiatorId   := 'ID. DEL PRESENTADOR';
      Norma3414.ChargeDate    := Date + 2;

      Norma3414.AddInitiator(Initiator);

      Norma3414.CreateFile('test-3414.xml');
      Norma3414.CloseFile;
      ShowMessage('Fichero 34.14 creado');
   finally
      Norma3414.Free;
   end;
end;

end.
