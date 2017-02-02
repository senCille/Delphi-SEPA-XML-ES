unit senCille.SEPAAuxClasses;

interface

uses System.Generics.Collections;

type
  //info de una orden de cobro (norma 19.14 xml)
  TsepaCollect = class {un Cobro}
    IdCobro         :string; //id unico cobro, ejemplo:20130930Fra.509301
    Importe         :Double;
    IdMandato       :string;
    DateOfSignature :TDateTime; //del mandato
    BIC             :string;
    NombreDeudor    :string;
    IBAN            :string;
    Concepto        :string;
  end;

  //un conjunto de cobros por Ordenante, lo utilizamos por si utilizan
  //cobros a ingresar en diferentes cuentas (el <PmtInf> contiene la info del Ordenante, con su cuenta; y los
  //cobros relacionados con este Ordenante/cuenta de abono
  TsepaInitiator = class {un Ordenante}
  private
    FCollects :TList<TsepaCollect>;
  public
    PaymentId       :string; //Ejemplo: 2013-10-28_095831Remesa 218 UNICO POR Ordenante
    NombreOrdenante :string;
    IBANOrdenante   :string;
    BICOrdenante    :string;
    {----------------------------------------------------------------------------------}
    IdOrdenante     :string; //el ID único del ordenante, normalmente dado por el banco
    {----------------------------------------------------------------------------------}
    constructor Create;
    destructor Destroy; override;
    function GetTotalImport:Double;
    property Collects :TList<TsepaCollect> read FCollects write FCollects;
  end;

  //info de una orden de pago (norma 34.14 xml)
  TsepaPayment = class
    IdPago             :string; //id unico pago, ejemplo:20130930Fra.509301
    Importe            :Double;
    BICBeneficiario    :string;
    NombreBeneficiario :string;
    IBANBeneficiario   :string;
    Concepto           :string;
  end;

  //un conjunto de pagos por Ordenante, lo utilizamos por si utilizan
  //pagos a cargar en diferentes cuentas (el <PmtInf> contiene la info del Ordenante, con su cuenta; y los
  //pagos relacionados con este Ordenante/cuenta de cargo
  TsepaOrdenante = class
    PaymentId       :string; //Ejemplo: 2013-10-28_095831Remesa 218 UNICO POR Ordenante
    SumaImportes    :Double;
    NombreOrdenante :string;
    IBANOrdenante   :string;
    BICOrdenante    :string;
    Payments        :TList<TsepaPayment>;
    constructor Create;
    destructor Destroy; reintroduce;
  end;

implementation

{ TsepaInitiator }

constructor TsepaInitiator.Create;
begin
   inherited;
   Collects := TList<TsepaCollect>.Create;
end;

destructor TsepaInitiator.Destroy;
begin
   Collects.Free;
   inherited;
end;

function TsepaInitiator.GetTotalImport: Double;
var i :TsepaCollect;
begin
   Result := 0;
   for i in FCollects do begin
      Result := Result + i.Importe;
   end;
end;

{ TInfoOrdenante }

constructor TsepaOrdenante.Create;
begin
   inherited;
   Payments := TList<TsepaPayment>.Create;
end;

destructor TsepaOrdenante.Destroy;
begin
   Payments.Free;
   inherited;
end;

end.
