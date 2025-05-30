unit IdeaL.Lib.Api.ReceitaWs;

{
2021-01-07
https://receitaws.com.br/api

https://receitaws.com.br/pricing
Analisando os precos
API Gratuita com Token permite 3 consultas por minuto
API Publica, nao identifiquei informacoes de limite
}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TAtividade = class
  private
    FDescricao: string;
    FCodigo: string;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AJson: string); overload;

    property Codigo: string read FCodigo;
    property Descricao: string read FDescricao;
    { public declarations }      
  published
    { published declarations }
  end;
  TEmpresa = class
  private
    FLogradouro: string;
    FFantasia: string;
    FCnpj: string;
    FBairro: string;
    FEmail: string;
    FUf: string;
    FNumero: string;
    FCidade: string;
    FDataAbertura: TDateTime;
    FDataSituacao: TDateTime;
    FSituacao: string;
    FComplemento: string;
    FUltimaAtualizacao: TDateTime;
    FRazaoSocial: string;
    FTipo: string;
    FNaturezaJuridica: string;
    FTelefone: string;
    FPorte: string;
    FCep: string;
    FStatus: string;
    FCapitalSocial: string;
    FAtividadeSecundaria: TArray<TAtividade>;
    FAtividadePrincipal: TArray<TAtividade>;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AJson: string); overload;
    destructor Destroy; override;

    property Situacao: string read FSituacao;
    property DataSituacao: TDateTime read FDataSituacao;    

    property DataAbertra: TDateTime read FDataAbertura;
    
    property Tipo: string read FTipo;
    property Cnpj: string read FCnpj;
    property Porte: string read FPorte;
    property CapitalSocial: string read FCapitalSocial;
    property RazaoSocial: string read FRazaoSocial; 
    property Fantasia: string read FFantasia;   
    property Telefone: string read FTelefone;
    property Email: string read FEmail;

    property Bairro: string read FBairro;
    property Logradouro: string read FLogradouro;
    property Numero: string read FNumero;
    property Cep: string read FCep;
    property Cidade: string read FCidade;
    property Complemento: string read FComplemento;
    property Uf: string read FUf; 

    property Status: string read FStatus;
    property NaturezaJuridica: string read FNaturezaJuridica; 
    property UltimaAtualizacao: TDateTime read FUltimaAtualizacao; 

    property AtividadePrincipal: TArray<TAtividade> read FAtividadePrincipal;
    property AtividadeSecundaria: TArray<TAtividade> read FAtividadeSecundaria;
    { public declarations }    
  published
    { published declarations }
  end;
  TReceitaWs = class
  private
    const
      CReceitaWSUrl = 'https://www.receitaws.com.br/v1';
      CReceitaWSUrlPublica = CReceitaWSUrl + '/cnpj/%s';
      CReceitaWSUrlComercial = CReceitaWSUrl + '/cnpj/%s/days/%d';
    { private declarations }
  protected
    { protected declarations }
  public
    class function Consultar(ACNPJ: string; ACallback: TProc = nil): string; overload;
    class function Consultar(AToken, ACNPJ: string; ADays: Integer = 365; ACallback: TProc = nil): string; overload;
    { public declarations }
  published
    { published declarations }
  end;

implementation

uses
  IdeaL.Lib.Utils;

{ TReceitaWs }

class function TReceitaWs.Consultar(AToken, ACNPJ: string; ADays: Integer; ACallback: TProc): string;
var
  LHttp: THTTPClient;
  LHeader: TNetHeaders;
  LUrl: string;
  LResponse: IHTTPResponse;
begin
  Result := EmptyStr;
  LHeader := [];

  if not AToken.Trim.IsEmpty then
  begin
    LUrl := Format(CReceitaWSUrlComercial, [ACNPJ, ADays]);
    LHeader := [
      TNameValuePair.Create('Authorization', 'Bearer ' + AToken)
    ];
  end
  else
  begin
    LUrl := Format(CReceitaWSUrlPublica, [ACNPJ]);
  end;

  LHttp := THTTPClient.Create;
  try
    LResponse := LHttp.Get(LUrl, nil, LHeader);
  finally
    FreeAndNil(LHttp);
  end;

  if LResponse.StatusCode <> 200 then
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText);

  Result := LResponse.ContentAsString(TEncoding.UTF8);  
end;

class function TReceitaWs.Consultar(ACNPJ: string; ACallback: TProc): string;
begin
  Result := Consultar('', ACNPJ, 365, ACallback);
end;

{ TEmpresa }

constructor TEmpresa.Create(AJson: string);
var
  LDataTimeStr: string;
  LCount: Integer;
  i: Integer;
  LJson: string;
begin    
  TUtils.GetParamValueFromJsonObject('tipo', AJson, FTipo, EmptyStr);
  TUtils.GetParamValueFromJsonObject('nome', AJson, FRazaoSocial, EmptyStr);  
  TUtils.GetParamValueFromJsonObject('telefone', AJson, FTelefone, EmptyStr);
  TUtils.GetParamValueFromJsonObject('email', AJson, FEmail, EmptyStr);
  TUtils.GetParamValueFromJsonObject('situacao', AJson, FSituacao, EmptyStr);
  TUtils.GetParamValueFromJsonObject('bairro', AJson, FBairro, EmptyStr);
  TUtils.GetParamValueFromJsonObject('logradouro', AJson, FLogradouro, EmptyStr);
  TUtils.GetParamValueFromJsonObject('numero', AJson, FNumero, EmptyStr);
  TUtils.GetParamValueFromJsonObject('cep', AJson, FCep, EmptyStr);
  TUtils.GetParamValueFromJsonObject('municipio', AJson, FCidade, EmptyStr);
  TUtils.GetParamValueFromJsonObject('porte', AJson, FPorte, EmptyStr);
  TUtils.GetParamValueFromJsonObject('capital_social', AJson, FCapitalSocial, EmptyStr);
  TUtils.GetParamValueFromJsonObject('natureza_juridica', AJson, FNaturezaJuridica, EmptyStr);
  TUtils.GetParamValueFromJsonObject('fantasia', AJson, FFantasia, EmptyStr);
  TUtils.GetParamValueFromJsonObject('cnpj', AJson, FCnpj, EmptyStr);  
  TUtils.GetParamValueFromJsonObject('status', AJson, FStatus, EmptyStr);  
  TUtils.GetParamValueFromJsonObject('complemento', AJson, FComplemento, EmptyStr);
  TUtils.GetParamValueFromJsonObject('uf', AJson, FUf, EmptyStr);

  TUtils.GetParamValueFromJsonObject('abertura', AJson, LDataTimeStr, EmptyStr);
  TUtils.TryStrToDateTime(LDataTimeStr, FDataAbertura);
  TUtils.GetParamValueFromJsonObject('ultima_atualizacao', AJson, LDataTimeStr, EmptyStr);
  TUtils.TryStrToDateTime(LDataTimeStr, FUltimaAtualizacao);
  TUtils.GetParamValueFromJsonObject('data_situacao', AJson, LDataTimeStr, EmptyStr);
  TUtils.TryStrToDateTime(LDataTimeStr, FDataSituacao);

  TUtils.GetParamValueFromJsonObject('atividade_principal', AJson, LJson, EmptyStr);
  if not(LJson.Trim.IsEmpty) then
  begin
    LCount := TUtils.GetJsonArraySize(LJson);
    SetLength(FAtividadePrincipal, LCount);
    for i := 0 to Pred(LCount) do
    begin
      FAtividadePrincipal[i] := TAtividade.Create(TUtils.GetJsonObjectFromJsonArray(LJson, i));
    end;
  end;

  TUtils.GetParamValueFromJsonObject('atividades_secundarias', AJson, LJson, EmptyStr);
  if not(LJson.Trim.IsEmpty) then
  begin
    LCount := TUtils.GetJsonArraySize(LJson);
    SetLength(FAtividadeSecundaria, LCount);
    for i := 0 to Pred(LCount) do
    begin
      FAtividadeSecundaria[i] := TAtividade.Create(TUtils.GetJsonObjectFromJsonArray(LJson, i));
    end;
  end;
end;

destructor TEmpresa.Destroy;
var
  i: Integer;
begin
  for i := 0 to Pred(Length(FAtividadePrincipal)) do
    FreeAndNil(FAtividadePrincipal[i]);
  SetLength(FAtividadePrincipal, 0);
  for i := 0 to Pred(Length(FAtividadeSecundaria)) do
    FreeAndNil(FAtividadeSecundaria[i]);
  SetLength(FAtividadeSecundaria, 0);
  inherited;
end;

{ TAtividade }

constructor TAtividade.Create(AJson: string);
begin
  TUtils.GetParamValueFromJsonObject('code', AJson, FCodigo, EmptyStr);
  TUtils.GetParamValueFromJsonObject('text', AJson, FDescricao, EmptyStr);
end;

end.
