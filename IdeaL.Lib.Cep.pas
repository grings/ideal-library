unit IdeaL.Lib.Cep;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.JSON,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TWsType = (wtCepAberto, wtOpenCep, wtViaCep, wtRepublicaVirtual);

type
  TCepRec = record
    Bairro: string;
    Cep: string;
    Cidade: string;
    Ddd: string;
    Logradouro: string;
    Ibge: string;
    Uf: string;
    UfId: Integer;

    procedure Clear;
    /// <summary> Expects a default JSON following the TCepRec structure
    /// </summary>
    procedure LoadFromJson(AJson: string); overload;
    /// <summary> Expects a JSON which follows the pattern of the WS sent
    /// </summary>
    procedure LoadFromJson(AJson: string; AWsType: TWsType); overload;
    function ToJson: string;
  end;

  TCep = class
  private
    const
    CWsIteration: array [0 .. 2] of TWsType = (wtOpenCep, wtViaCep, wtRepublicaVirtual);
    CUf: array [0 .. 27] of string =
      ('EX', 'RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN',
      'PB', 'PE', 'AL', 'SE', 'BA', 'MG', 'ES', 'RJ', 'SP', 'PR', 'SC', 'RS',
      'MS', 'MT', 'GO', 'DF');
    CUfId: array [0 .. 27] of Integer =
      (0, 11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53);

    class function GetHttp(AUrl: string): string;

    class function Get(ACep: string; AIteration: Integer; AError: string): string; overload;

    class function UfToId(AUf: string): Integer;
    class function UfIdToUf(AUf: Integer): string;
    { private declarations }
  public
    /// <summary> Gets CEP recursively from all Free WSs registred
    /// </summary>
    /// <param name="ACep">CEP to be looked on the WS
    /// </param>
    /// <remarks>
    /// More info on their website
    /// <see cref="http://viacep.com.br/"/>
    /// </remarks>
    /// <returns>Retunrs its native pattern JSON
    /// </returns>
    class function Get(ACep: string): string; overload;

    /// <summary> Gets CEP from WebService ViaCEP, Free API
    /// </summary>
    /// <param name="ACep">CEP to be looked on the WS
    /// </param>
    /// <remarks>
    /// More info on their website
    /// <see cref="http://viacep.com.br/"/>
    /// </remarks>
    /// <returns>Retunrs its native pattern JSON
    /// </returns>
    class function GetViaCep(ACep: string): string;
    /// <summary> Gets CEP from WebService CEPAberto. Needs to be registred
    /// </summary>
    /// <param name="ACep">CEP to be looked on the WS
    /// </param>
    /// <param name="AToken">Token that garants you access to the API
    /// </param>
    /// <remarks>
    /// You're gonna need a Token that can be got from their website
    /// <see cref="https://cepaberto.com/"/>
    /// </remarks>
    /// <returns>Retunrs its native pattern JSON
    /// </returns>
    class function GetCepAberto(ACep, AToken: string): string;
    /// <summary> Gets CEP from WebService OpenCep, Free API
    /// </summary>
    /// <param name="ACep">CEP to be looked on the WS
    /// </param>
    /// <remarks>
    /// More info on their website
    /// <see cref="https://opencep.com/"/>
    /// </remarks>
    /// <returns>Retunrs its native pattern JSON
    /// </returns>
    class function GetOpenCep(ACep: string): string;
    class function GetRepublicaVirtual(ACep: string): string;

    class function CepAbertoToJson(AJson: string): string;
    class function ViaCepToJson(AJson: string): string;
    class function RepublicaVirtualToJson(AJson: string): string;
  type
    TRecord = TCepRec;
    { public declarations }
  end;

implementation

{ TCep }

class function TCep.CepAbertoToJson(AJson: string): string;
var
  LJsonObj: TJSONObject;
  LJsonValue: TJSONValue;
  LJsonCidade: TJSONObject;
  LJsonUf: TJSONObject;

  LJsonResp: TJSONObject;
  LValue: string;
begin
  Result := EmptyStr;
  LJsonCidade := nil;
  LJsonObj := TJSONObject.Create;
  LJsonResp := TJSONObject.Create;
  try

    LJsonValue := LJsonObj.ParseJSONValue(AJson);

    if not(Assigned(LJsonValue)) then
      raise Exception.Create('Invalid JSON');

    if LJsonValue.TryGetValue<string>('bairro', LValue) then
      LJsonResp.AddPair('bairro', LValue);
    if LJsonValue.TryGetValue<string>('cep', LValue) then
      LJsonResp.AddPair('cep', StringReplace(LValue, '-', EmptyStr, [rfReplaceAll]));
    if LJsonValue.TryGetValue<TJSONObject>('cidade', LJsonCidade) then
    begin
      if LJsonCidade.TryGetValue<string>('nome', LValue) then
        LJsonResp.AddPair('cidade', LValue);
      if LJsonCidade.TryGetValue<string>('ddd', LValue) then
        LJsonResp.AddPair('ddd', LValue);
      if LJsonCidade.TryGetValue<string>('ibge', LValue) then
        LJsonResp.AddPair('ibge', LValue);
    end;

    if LJsonValue.TryGetValue<string>('logradouro', LValue) then
      LJsonResp.AddPair('logradouro', LValue);

    if LJsonValue.TryGetValue<TJSONObject>('estado', LJsonUf) then
    begin
      LValue := EmptyStr;
      if LJsonUf.TryGetValue<string>('sigla', LValue) then
      begin
        LJsonResp.AddPair('uf', LValue);
        LJsonResp.AddPair('ufid', TJSONNumber.Create(UfToId(LValue)));
      end;
    end;

    Result := LJsonResp.ToString;
  finally
    FreeAndNil(LJsonValue);
    FreeAndNil(LJsonObj);
    FreeAndNil(LJsonResp);
  end;
end;

class function TCep.Get(ACep: string): string;
begin
  Result := Get(ACep, 0, EmptyStr);
end;

class function TCep.Get(ACep: string; AIteration: Integer; AError: string): string;
begin
  try
    case AIteration of
      0:
        begin
          Result := GetViaCep(ACep);
          Result := ViaCepToJson(Result);
        end;
      1:
        begin
          Result := GetOpenCep(ACep);
          // until 2022.04.21 the JSON result is compatible
          Result := ViaCepToJson(Result);
        end;
      2:
        begin
          Result := GetRepublicaVirtual(ACep);
          Result := RepublicaVirtualToJson(Result);
        end;
      { implement more free WSs here }
    else
      raise Exception.Create(
        'Unknow WebService. Tried all known WSs' + sLineBreak +
        AError);
    end;
  except
    on E: Exception do
    begin
      // If iteration is equal the number of WS on iteration Array, then, we tried all WSs
      if AIteration >= (Length(CWsIteration)) then
        raise Exception.Create('TCep ' + E.Message);
      Result := Get(ACep, AIteration + 1, Trim(AError + sLineBreak + AIteration.ToString + ': ' + E.Message));
    end;
  end;
end;

class function TCep.GetCepAberto(ACep, AToken: string): string;
var
  LHttpCli: TNetHTTPClient;
  LResponse: IHTTPResponse;
  LHeader: TNetHeaders;
begin
  LHttpCli := TNetHTTPClient.Create(nil);
  try
    SetLength(LHeader, 1);

    LHeader[0] := TNameValuePair.Create('Authorization', 'Token token=' + AToken);

    LResponse := LHttpCli.Get(
      'https://www.cepaberto.com/api/v3/cep?cep=' + ACep,
      nil,
      LHeader);
  finally
    FreeAndNil(LHttpCli);
  end;

  // The endpoint responses 200 if the CEP wasn't found
  if (LResponse.StatusCode <> 200) then
  begin
    raise Exception.Create('TCep.GetCepAberto ' + LResponse.StatusCode.ToString + ' ' + LResponse.StatusText)
  end
  else
  begin
    Result := LResponse.ContentAsString(TEncoding.UTF8);

    if LowerCase(Result).Contains('"erro":') then
      raise Exception.Create('TCep.GetCepAberto ' + Result)
  end;
end;

class function TCep.GetHttp(AUrl: string): string;
var
  LHttpCli: TNetHTTPClient;
  LResponse: IHTTPResponse;
begin
  LHttpCli := TNetHTTPClient.Create(nil);
  try
    LResponse := LHttpCli.Get(AUrl);
  finally
    FreeAndNil(LHttpCli);
  end;

  // The endpoint responses 200 if the CEP wasn't found
  if (LResponse.StatusCode <> 200) then
  begin
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText)
  end;

  Result := LResponse.ContentAsString(TEncoding.UTF8);
end;

class function TCep.GetOpenCep(ACep: string): string;
begin
  Result := GetHttp('http://opencep.com/v1/' + ACep);
end;

class function TCep.GetRepublicaVirtual(ACep: string): string;
var
  LJsValue: TJSONValue;
begin
  Result := GetHttp('http://cep.republicavirtual.com.br/web_cep.php?cep=' + ACep + '&formato=json');
  if LowerCase(Result).Contains('nao encontrado') then
    raise Exception.Create('TCep.GetRepublicaVirtual ' + Result);

  LJsValue := TJSONObject.ParseJSONValue(Result);
  try
    TJSONObject(LJsValue).AddPair('cep', ACep);

    Result := LJsValue.ToString;
  finally
    FreeAndNil(LJsValue);
  end;
end;

class function TCep.GetViaCep(ACep: string): string;
begin
  Result := GetHttp('http://viacep.com.br/ws/' + ACep + '/json/');

  if LowerCase(Result).Contains('"erro":') then
    raise Exception.Create('TCep.GetViaCep ' + Result)
end;

class function TCep.RepublicaVirtualToJson(AJson: string): string;
var
  LJsonObj: TJSONObject;
  LJsonValue: TJSONValue;

  LJsonResp: TJSONObject;
  LValue: string;
  LValue1: string;
begin
  Result := EmptyStr;
  LJsonObj := TJSONObject.Create;
  LJsonResp := TJSONObject.Create;
  try

    LJsonValue := LJsonObj.ParseJSONValue(AJson);

    if not(Assigned(LJsonValue)) then
      raise Exception.Create('Invalid JSON');

    if LJsonValue.TryGetValue<string>('bairro', LValue) then
      LJsonResp.AddPair('bairro', LValue);
    if LJsonValue.TryGetValue<string>('cep', LValue) then
      LJsonResp.AddPair('cep', StringReplace(LValue, '-', EmptyStr, [rfReplaceAll]));
    if LJsonValue.TryGetValue<string>('cidade', LValue) then
      LJsonResp.AddPair('cidade', LValue);
    if LJsonValue.TryGetValue<string>('logradouro', LValue) then
    begin
      LJsonValue.TryGetValue<string>('tipo_logradouro', LValue1);
      LJsonResp.AddPair('logradouro', Trim(LValue1 + ' ' + LValue));
    end;
    if LJsonValue.TryGetValue<string>('uf', LValue) then
      LJsonResp.AddPair('uf', LValue);
    LJsonResp.AddPair('ufid', TJSONNumber.Create(UfToId(LValue)));

    Result := LJsonResp.ToString;
  finally
    FreeAndNil(LJsonValue);
    FreeAndNil(LJsonObj);
    FreeAndNil(LJsonResp);
  end;
end;

class function TCep.UfIdToUf(AUf: Integer): string;
begin
  Result := EmptyStr;
  for var i := Low(CUfId) to High(CUfId) do
  begin
    if CUfId[i] = AUf then
    begin
      Result := CUf[i];
      Break;
    end;
  end;
end;

class function TCep.UfToId(AUf: string): Integer;
begin
  Result := -1;
  for var i := Low(CUf) to High(CUf) do
  begin
    if LowerCase(CUf[i]).Equals(LowerCase(AUf)) then
    begin
      Result := CUfId[i];
      Break;
    end;
  end;
end;

class function TCep.ViaCepToJson(AJson: string): string;
var
  LJsonObj: TJSONObject;
  LJsonValue: TJSONValue;

  LJsonResp: TJSONObject;
  LValue: string;
begin
  Result := EmptyStr;
  LJsonObj := TJSONObject.Create;
  LJsonResp := TJSONObject.Create;
  try

    LJsonValue := LJsonObj.ParseJSONValue(AJson);

    if not(Assigned(LJsonValue)) then
      raise Exception.Create('Invalid JSON');

    if LJsonValue.TryGetValue<string>('bairro', LValue) then
      LJsonResp.AddPair('bairro', LValue);
    if LJsonValue.TryGetValue<string>('cep', LValue) then
      LJsonResp.AddPair('cep', StringReplace(LValue, '-', EmptyStr, [rfReplaceAll]));
    if LJsonValue.TryGetValue<string>('localidade', LValue) then
      LJsonResp.AddPair('cidade', LValue);
    if LJsonValue.TryGetValue<string>('ddd', LValue) then
      LJsonResp.AddPair('ddd', LValue);
    if LJsonValue.TryGetValue<string>('logradouro', LValue) then
      LJsonResp.AddPair('logradouro', LValue);
    if LJsonValue.TryGetValue<string>('ibge', LValue) then
      LJsonResp.AddPair('ibge', LValue);
    if LJsonValue.TryGetValue<string>('uf', LValue) then
      LJsonResp.AddPair('uf', LValue);
    LJsonResp.AddPair('ufid', TJSONNumber.Create(UfToId(LValue)));

    Result := LJsonResp.ToString;
  finally
    FreeAndNil(LJsonValue);
    FreeAndNil(LJsonObj);
    FreeAndNil(LJsonResp);
  end;
end;

{ TCepRec }

procedure TCepRec.Clear;
begin
  Bairro := EmptyStr;;
  Cep := EmptyStr;;
  Cidade := EmptyStr;;
  Ddd := EmptyStr;;
  Logradouro := EmptyStr;;
  Ibge := EmptyStr;;
  Uf := EmptyStr;;
  UfId := -1;
end;

procedure TCepRec.LoadFromJson(AJson: string; AWsType: TWsType);
var
  LJson: string;
begin
  case AWsType of
    wtCepAberto: LJson := TCep.CepAbertoToJson(AJson);
    wtViaCep, wtOpenCep: LJson := TCep.ViaCepToJson(AJson);
  end;

  LoadFromJson(LJson);
end;

function TCepRec.ToJson: string;
var
  LJsonResp: TJSONObject;
  LValue: string;
begin
  Result := EmptyStr;
  LJsonResp := TJSONObject.Create;
  try
    LJsonResp.AddPair('bairro', Bairro);
    LJsonResp.AddPair('cep', Cep);
    LJsonResp.AddPair('cidade', Cidade);
    LJsonResp.AddPair('ddd', Ddd);
    LJsonResp.AddPair('logradouro', Logradouro);
    LJsonResp.AddPair('ibge', Ibge);
    LJsonResp.AddPair('uf', Uf);
    LJsonResp.AddPair('ufid', TJSONNumber.Create(UfId));

    Result := LJsonResp.ToString;
  finally
    FreeAndNil(LJsonResp);
  end;
end;

procedure TCepRec.LoadFromJson(AJson: string);
var
  LJsonObj: TJSONObject;
  LJsonValue: TJSONValue;
  LValue: string;
begin
  Clear;

  LJsonObj := TJSONObject.Create;
  try

    LJsonValue := LJsonObj.ParseJSONValue(AJson);

    if not(Assigned(LJsonValue)) then
      raise Exception.Create('Invalid JSON');

    if LJsonValue.TryGetValue<string>('bairro', LValue) then
      Bairro := LValue;
    if LJsonValue.TryGetValue<string>('cep', LValue) then
      Cep := StringReplace(LValue, '-', EmptyStr, [rfReplaceAll]);
    if LJsonValue.TryGetValue<string>('cidade', LValue) then
      Cidade := LValue;
    if LJsonValue.TryGetValue<string>('ddd', LValue) then
      Ddd := LValue;
    if LJsonValue.TryGetValue<string>('logradouro', LValue) then
      Logradouro := LValue;
    if LJsonValue.TryGetValue<string>('ibge', LValue) then
      Ibge := LValue;
    if LJsonValue.TryGetValue<string>('uf', LValue) then
    begin
      Uf := LValue;
      UfId := TCep.UfToId(LValue);
    end;

  finally
    FreeAndNil(LJsonValue);
    FreeAndNil(LJsonObj);
  end;
end;

end.
