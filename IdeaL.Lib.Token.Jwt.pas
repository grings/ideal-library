unit IdeaL.Lib.Token.Jwt;

interface

uses
{$IFDEF VCL}
  JOSE.Core.JWK,
  JOSE.Core.Jwt,
  JOSE.Core.JWS,
  JOSE.Types.JSON,
  JOSE.Core.JWA,
{$ENDIF}
  System.SysUtils;

type
  TTokenJwt = class
  private
    { private declarations }
  protected
    { protected declarations }
  public
    type
    TTokenType = (ttAuth, ttRefresh);

{$IFDEF VCL}
  class function GenerateKeyPair(LAlg: TJOSEAlgorithmId; ASignature: string; ASignaturePrivate: string = ''): TKeyPair;
  class function CreateToken(AHeader: string; const APayload, AKey: string; LAlg: TJOSEAlgorithmId = TJOSEAlgorithmId.HS256): string;

  class function Verify(const AKey, AToken: string; out AHeader, APayload, ASignature: string): Boolean; overload;
  class function Verify(const AKey, AToken: string; out AHeader, APayload: string): Boolean; overload;
  class function Verify(const AKey, AToken: string): Boolean; overload;
{$ENDIF}
  class procedure ExtractData(const AToken: string; out APayload: string); overload;
  class procedure ExtractData(const AToken: string; out AHeader, APayload: string); overload;
  class procedure ExtractData(const AToken: string; out AHeader, APayload, ASignature: string); overload;
  { public declarations }
  end;

implementation

{ TTokenJwt }

{$IFDEF VCL}

class function TTokenJwt.CreateToken(AHeader: string; const APayload,
  AKey: string; LAlg: TJOSEAlgorithmId): string;
var
  LJWT: TJWT;
  LSigner: TJWS;
  LKeyPair: TKeyPair;
begin
  LSigner := nil;
  LKeyPair := nil;
  LJWT := TJWT.Create(TJWTClaims);

  try
    if AHeader.Trim.IsEmpty then
      AHeader := '{"alg": "' + LAlg.AsString + '","typ": "JWT"}';
    LJWT.Header.JSON.Free;
    LJWT.Header.JSON := TJSONObject(TJSONObject.ParseJSONValue(AHeader));

    LJWT.Claims.JSON.Free;
    LJWT.Claims.JSON := TJSONObject(TJSONObject.ParseJSONValue(APayload));

    LSigner := TJWS.Create(LJWT);
    LKeyPair := GenerateKeyPair(LAlg, AKey);

    LSigner.SkipKeyValidation := True;
    LSigner.Sign(LKeyPair.PrivateKey, LAlg);
    Result := LSigner.Header + '.' + LSigner.Payload + '.' + LSigner.Signature;
  finally
    FreeAndNil(LKeyPair);
    FreeAndNil(LSigner);
    FreeAndNil(LJWT);
  end;
end;

class function TTokenJwt.GenerateKeyPair(LAlg: TJOSEAlgorithmId; ASignature, ASignaturePrivate: string): TKeyPair;
begin
  Result := TKeyPair.Create;

  if ASignaturePrivate.Trim.IsEmpty then
    ASignaturePrivate := ASignature;

  Result.PrivateKey.Key := ASignature;
  Result.PublicKey.Key := ASignaturePrivate;
end;

class function TTokenJwt.Verify(const AKey, AToken: string; out AHeader,
  APayload, ASignature: string): Boolean;
var
  LToken: TJWT;
  LSigner: TJWS;
  LKey: TJWK;
begin
  Result := False;

  AHeader := EmptyStr;
  APayload := EmptyStr;
  ASignature := EmptyStr;

  LKey := TJWK.Create(AKey);
  LToken := TJWT.Create;
  try
    try
      LSigner := TJWS.Create(LToken);
      LSigner.SkipKeyValidation := True;
      try
        LSigner.SetKey(LKey);
        LSigner.CompactToken := AToken;
        if LSigner.VerifySignature then
        begin
          AHeader := LSigner.Header;
          APayload := LSigner.Payload;
          ASignature := LSigner.Signature;
        end;
      finally
        FreeAndNil(LSigner);
      end;
      Result := LToken.Verified;
    finally
      FreeAndNil(LKey);
      FreeAndNil(LToken);
    end;
  except

  end;
end;

class function TTokenJwt.Verify(const AKey, AToken: string; out AHeader,
  APayload: string): Boolean;
var
  LSignature: string;
begin
  Result := Verify(AKey, AToken, AHeader, APayload, LSignature);
end;

class function TTokenJwt.Verify(const AKey, AToken: string): Boolean;
begin
  Result := Verify(AKey, AToken);
end;

{$ENDIF}

class procedure TTokenJwt.ExtractData(const AToken: string; out AHeader,
  APayload: string);
var
  LSignature: string;
begin
  ExtractData(AToken, AHeader, APayload, LSignature);
end;

class procedure TTokenJwt.ExtractData(const AToken: string;
  out APayload: string);
var
  LHeader: string;
  LSignature: string;
begin
  ExtractData(AToken, LHeader, APayload, LSignature);
end;

class procedure TTokenJwt.ExtractData(const AToken: string; out AHeader,
  APayload, ASignature: string);
{$IFDEF VCL}
var
  LToken: TJWT;
  LSigner: TJWS;
{$ENDIF}
begin
  AHeader := EmptyStr;
  APayload := EmptyStr;
  ASignature := EmptyStr;

{$IFDEF VCL}
  LToken := TJWT.Create;
  try
    try
      LSigner := TJWS.Create(LToken);
      LSigner.SkipKeyValidation := True;
      try
        LSigner.CompactToken := AToken;
        AHeader := LSigner.Header;
        APayload := LSigner.Payload;
        ASignature := LSigner.Signature;
      finally
        FreeAndNil(LSigner);
      end;
    finally
      FreeAndNil(LToken);
    end;
  except

  end;
{$ELSE}
  var
  LPontoCount := AToken.CountChar('.');
  if not AToken.CountChar('.') = 2 then
    raise Exception.Create('Invalid JWT value - ' + AToken);
  var
  LPos1 := 0;
  var
  LPos2 := 0;
  LPos2 := Pos('.', AToken);
  AHeader := Copy(AToken, LPos1, LPos2).Trim(['.']);
  LPos1 := Pos('.', AToken, LPos2 + 2);
  APayload := Copy(AToken, LPos2, LPos1 - LPos2).Trim(['.']);
  ASignature := Copy(AToken, LPos1, AToken.Length).Trim(['.']);
{$ENDIF}
end;

end.
