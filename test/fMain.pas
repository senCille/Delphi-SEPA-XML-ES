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
    Ordenante :TsepaInitiator;
    Collect   :TsepaCollect;
begin
   Norma1914 := TDJMNorma1914XML.Create;
   try
      {Información general del fichero}
      {El id. del identificador te lo dará el Banco, LEER LAS NORMAS SEPA!!}
      Norma1914.FileDate      := Date;
      Norma1914.InitiatorName := 'NOMBRE DEL PRESENTADOR S.L.';
      Norma1914.InitiatorId   := 'ID.PRESENTADOR';
      Norma1914.ChargeDate    := Date + 2;

      {Un Ordenante. Un presentador puede presentar las órdenes de varios ordenantes}
      Ordenante := TsepaInitiator.Create;
      Ordenante.IdOrdenante     := 'ID UNICO DE LAS ORDENES DE ESTE ORDENANTE';
      Ordenante.NombreOrdenante := 'EMPRESA ORDENANTE 1 S.L.'                 ;
      Ordenante.IBANOrdenante   := 'el iban de este ordenante'                ;
      Ordenante.BICOrdenante    := 'BicOrdenante1'                            ;
      {----------------------------------------------------------------------------------}
      Ordenante.IdOrdenante     := 'ID.ORDENANTE'; {el ID único del ordenante, normalmente dado por el banco}

      //las ordenes de cobro de este ordenante
      Collect := TsepaCollect.Create;
      Collect.IdCobro         := 'idCobro unico'; {utilizar un nº de documento o contador, etc}
      Collect.Importe         := 1200;
      Collect.IdMandato       := 'id.mandato'; {por ejemplo, poner un nº de contrato o del documento de la firma del mandato}
      Collect.DateOfSignature := StrToDate('31/10/2009'); {esta es la fecha por defecto de los que no tienen fecha de mandato}
      Collect.BIC             := 'Bic de este Deudor';
      Collect.NombreDeudor    := 'Nombre Deudor 1 S.L.';
      Collect.IBAN            := 'IBAN De este Deudor';
      Collect.Concepto        := 'Cobro de pruebas factura nº 1';
      Ordenante.Collects.Add(Collect);

      {Segunda Orden de Cobro}
      Collect := TsepaCollect.Create;
      Collect.IdCobro         := 'idCobro unico-2'              ; {utilizar un nº de documento o contador, etc}
      Collect.Importe         := 230                            ;
      Collect.IdMandato       := 'id.mandato'                   ;
      Collect.DateOfSignature := StrToDate('31/10/2009')        ; //esta es la fecha por defecto de los que no tienen fecha de mandato
      Collect.BIC             := 'Bic de este Deudor'           ;
      Collect.NombreDeudor    := 'Nombre Deudor 2 S.L.'         ;
      Collect.IBAN            := 'IBAN De este Deudor'          ;
      Collect.Concepto        := 'Cobro de pruebas factura nº 1';
      Ordenante.Collects.Add(Collect);

      Norma1914.AddOrdenante(Ordenante);

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
begin
   Norma3414:=TDJMNorma3414XML.create;
   try
      {info del presentador}
      Norma3414.FileDate      := Date;
      Norma3414.InitiatorName := 'NOMBRE DEL PRESENTADOR';
      Norma3414.InitiatorId   := 'ID. DEL PRESENTADOR';
      Norma3414.ChargeDate    := Date + 2;
      {info del ordenante}
      Norma3414.AddOrdenante('ID. UNICO DEL PAGO'        ,
                              'NOMBRE DEL ORDENANTE S.L.',
                              'IBAN DEL ORDENANTE'       ,
                              'BIC DEL ORDENANTE'        );
      {Una orden de pago}
      Norma3414.AddPago('ID UNICO DEL PAGO'              ,
                         500                             ,
                         'BIC DEL BENEFICIARIO'          ,
                         'NOMBRE DEL BENEFICIARIO 1 S.L.',
                         'IBAN DEL BENEFICIARIO'         ,
                         'Pago de su factura nº 5698'    ,
                         'IBAN DEL ORDENANTE'            );{El mismo que añadimos con el ordenante}
      Norma3414.CreateFile('test-3414.xml');
      Norma3414.CloseFile;
      ShowMessage('Fichero 34.14 creado');
   finally
      Norma3414.Free;
   end;
end;

end.
