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

uses uDJMSepa1914XML,
     uDJMSepa3414XML;

{$R *.dfm}

procedure TfrMain.btnTestNorma19Click(Sender: TObject);
var Norma1914 :TDJMNorma1914XML;
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
      Norma1914.AddOrdenante('ID UNICO DE LAS ORDENES DE ESTE ORDENANTE',
                              'EMPRESA ORDENANTE 1 S.L.'                 ,
                              'el iban de este ordenante'                ,
                              'BicOrdenante1'                            ,
                              'ID.ORDENANTE'                             );
      //las ordenes de cobro de este ordenante
      Norma1914.AddCobro('idCobro unico', {utilizar un nº de documento o contador, etc}
                          1200,
                          'id.mandato', {por ejemplo, poner un nº de contrato o del documento de la firma del mandato}
                          StrToDate('31/10/2009')        , {esta es la fecha por defecto de los que no tienen fecha de mandato}
                          'Bic de este Deudor'           ,
                          'Nombre Deudor 1 S.L.'         ,
                          'IBAN De este Deudor'          ,
                          'Cobro de pruebas factura nº 1',
                          'el iban de este ordenante'    ); {importante, el cobro se coloca en su ordenante/cuenta de cobro}

      {Segunda Orden de Cobro}
      Norma1914.AddCobro('idCobro unico-2'              , {utilizar un nº de documento o contador, etc}
                          230                            ,
                          'id.mandato'                   ,
                          StrToDate('31/10/2009')        , //esta es la fecha por defecto de los que no tienen fecha de mandato
                          'Bic de este Deudor'           ,
                          'Nombre Deudor 2 S.L.'         ,
                          'IBAN De este Deudor'          ,
                          'Cobro de pruebas factura nº 1',
                          'el iban de este ordenante'    );//importante, el cobro se coloca en su ordenante/cuenta de cobro

      if Norma1914.HayCobros then begin {en algún algoritmo te puede ser util comprobar que has añadido cobros}
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
      Norma3414.AddOrdenante('ID. UNICO DEL PAGO',
                              'NOMBRE DEL ORDENANTE S.L.',
                              'IBAN DEL ORDENANTE',
                              'BIC DEL ORDENANTE'
                              );
      {Una orden de pago}
      Norma3414.AddPago('ID UNICO DEL PAGO'             ,
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
