unit senCille.SEPAAuxClasses;

interface

uses System.Generics.Collections;

type
  //info de una orden de cobro (norma 19.14 xml)
  TsepaOperation = class {un Cobro}
    OpId            :string; //id unico cobro, ejemplo:20130930Fra.509301
    Import          :Double;
    Concept         :string;
    BIC             :string;
    IBAN            :string;
    Name            :string;
    {--- exclusive for Collection ---}
    IdMandator      :string;
    DateOfSignature :TDateTime; {of the mandator}
  end;

  //un conjunto de cobros por Ordenante, lo utilizamos por si utilizan
  //cobros a ingresar en diferentes cuentas (el <PmtInf> contiene la info del Ordenante, con su cuenta; y los
  //cobros relacionados con este Ordenante/cuenta de abono
  TsepaInitiator = class {un Ordenante}
  private
    FOperations :TList<TsepaOperation>; {Collects}
  public
    PaymentId :string; //Ejemplo: 2013-10-28_095831Remesa 218 UNICO POR Ordenante
    Name      :string;
    IBAN      :string;
    BIC       :string;
    {----------------------------------------------------------------------------------}
    IdOrdenante :string; //el ID único del ordenante, normalmente dado por el banco
    {----------------------------------------------------------------------------------}
    constructor Create;
    destructor Destroy; override;
    function GetTotalImport:Double;
    property Operations :TList<TsepaOperation> read FOperations write FOperations;
  end;

  //un conjunto de pagos por Ordenante, lo utilizamos por si utilizan
  //pagos a cargar en diferentes cuentas (el <PmtInf> contiene la info del Ordenante, con su cuenta; y los
  //pagos relacionados con este Ordenante/cuenta de cargo
  TsepaOrdenante = class
  private
    FOperations :TList<TsepaOperation>; {Payments}
  public
    PaymentId :string; //Ejemplo: 2013-10-28_095831Remesa 218 UNICO POR Ordenante
    Name      :string;
    IBAN      :string;
    BIC       :string;

    constructor Create;
    destructor Destroy; reintroduce;
    function GetTotalImport:Double;
    property Operations :TList<TsepaOperation> read FOperations write FOperations;
  end;

implementation

{ TsepaInitiator }

constructor TsepaInitiator.Create;
begin
   inherited;
   FOperations := TList<TsepaOperation>.Create;
end;

destructor TsepaInitiator.Destroy;
begin
   Operations.Free;
   inherited;
end;

function TsepaInitiator.GetTotalImport: Double;
var i :TsepaOperation;
begin
   Result := 0;
   for i in FOperations do begin
      Result := Result + i.Import;
   end;
end;

{ TInfoOrdenante }

constructor TsepaOrdenante.Create;
begin
   inherited;
   FOperations := TList<TsepaOperation>.Create;
end;

destructor TsepaOrdenante.Destroy;
begin
   FOperations.Free;
   inherited;
end;

function TsepaOrdenante.GetTotalImport: Double;
var i :TsepaOperation;
begin
   Result := 0;
   for i in FOperations do begin
      Result := Result + i.Import;
   end;
end;

end.
