unit IdeaL.Lib.Encryption;

interface

uses
  System.Hash,
  System.SysUtils,
  System.Classes,
  System.Zip,

  System.NetEncoding,

  IdCoderMIME,
  IdHashMessageDigest,
  IdURI;

Type
  TEncryption = Class
  private
    { private fields }
    { private methods }
  public
    { public fields }
    class function GetHashSHA2(AValue: string; const ASal: string = ''): string;
    class function GetRandomString(const ALength: integer): String;
    class function Encode64(AValue: string): string; overload;
    class function Encode64(AValue: TStream): string; overload;
    class function Encode64FromFile(AFileName: string): string;
    class function Decode64(AValue: string): string;
    class function Decode64ToStream(AValue: string): TStream; overload;
    class procedure Decode64ToStream(AValue: string; AStream: TStream); overload;
    class function Decode64ToFile(AValue, AFileName: string): string;
    class function IsBase64(AValue: string): Boolean;
    class function MD5FromStream(const AValue: TStream): string;
    class function MD5FromFile(const AValue: string): string;
    class function MD5FromString(const AValue: string): string;
    class function EncryptStr(const AValue: string; AKey: string): string; overload;
    class function EncryptStr(const AValue: string; AKey: Word): string; overload;
    class function DecryptStr(const AValue: string; AKey: string): string; overload;
    class function DecryptStr(const AValue: string; AKey: Word): string; overload;
    class function UrlDecode(const AValue: string): string;
    class function UrlEncode(const AValue: string): string;
    { public methods }
  protected
    { protected fields }
    { protected methods }
  end;

implementation

uses
  System.RegularExpressions;

{ TEncryption }

class function TEncryption.Decode64(AValue: string): string;
begin
  Result := TNetEncoding.Base64.Decode(AValue);
  { var
    LDecoder: TIdDecoderMime;
    begin
    LDecoder := TIdDecoderMime.Create(nil);
    try
    Result := LDecoder.DecodeString(AValue);
    finally
    FreeAndNil(LDecoder);
    end; }
end;

class function TEncryption.DecryptStr(const AValue: string;
  AKey: string): string;
var
  i: integer;
  LChar: Char;
begin
  Result := '';
  for i := 0 to Pred(AValue.Length div 2) do
  begin
    LChar := Char(StrToIntDef('$' + AValue.SubString((i * 2), 2), Ord(' ')));
    if Length(AKey) > 0 then
      LChar := Char(Byte(AKey[1 + (i mod AKey.Length)]) xor Byte(LChar));
    Result := Result + LChar;
  end;
  { var
    KeyLen, KeyPos, OffSet, SrcPos, SrcAsc, TmpSrcAsc, Range: Integer;
    Dest: String;
    begin
    Result := '';
    if AValue <> '' then
    begin
    Dest := '';
    KeyLen := Length(AKey);
    KeyPos := 0;
    SrcPos := 0;
    SrcAsc := 0;
    Range := 256;

    OffSet := StrToInt('$' + copy(AValue, 1, 2));
    SrcPos := 3;
    repeat
    SrcAsc := StrToInt('$' + copy(AValue, SrcPos, 2));
    if (KeyPos < KeyLen) then
    KeyPos := KeyPos + 1
    else
    KeyPos := 1;
    TmpSrcAsc := SrcAsc Xor Ord(AKey.ToCharArray[KeyPos]);
    if TmpSrcAsc <= OffSet then
    TmpSrcAsc := 255 + TmpSrcAsc - OffSet
    else
    TmpSrcAsc := TmpSrcAsc - OffSet;
    Dest := Dest + Chr(TmpSrcAsc);
    OffSet := SrcAsc;
    SrcPos := SrcPos + 2;
    until (SrcPos >= Length(AValue));

    Result := Dest;
    end; }
end;

class function TEncryption.IsBase64(AValue: string): Boolean;
{$IF CompilerVersion < 35.0}
const
  CBase64Chars: Set of AnsiChar = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '+', '/', '='];
var
  i: integer;
{$ENDIF}
begin
{$IF CompilerVersion >= 35.0}
  Result := TRegEx.IsMatch(AValue, '^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$');
{$ELSE}
  Result := Length(AValue) mod 4 = 0;
  if Result then
  begin
    for i := 1 to Length(AValue) do
    begin
{$IFDEF UNICODE}
      if not CharInSet(AValue[i], CBase64Chars) then
{$ELSE}
      if not(AValue[i] in CBase64Chars) then
{$ENDIF}
      begin
        Result := False;
        Break;
      end;
    end;
  end;
{$ENDIF}
end;

class function TEncryption.Decode64ToStream(AValue: string): TStream;
var
  LDecoder: TIdDecoderMime;
begin
  { LDecoder := TIdDecoderMime.Create(nil);
    Result := TStream.Create;
    try
    LDecoder.DecodeStream(AValue, Result);
    finally
    FreeAndNil(LDecoder);
    end; }
  LDecoder := TIdDecoderMime.Create(nil);
  Result := TStream.Create;
  try
    try
      Decode64ToStream(AValue, Result);
    except
      Result.Free;
      raise;
    end;
  finally
    FreeAndNil(LDecoder);
  end;
end;

class function TEncryption.Decode64ToFile(AValue, AFileName: string): string;
var
  LStream: TStream;
begin
  LStream := Decode64ToStream(AValue);
  try
    TBytesStream(LStream).SaveToFile(AFileName);
  finally
    LStream.Free;
  end;
end;

class procedure TEncryption.Decode64ToStream(AValue: string; AStream: TStream);
var
  LDecoder: TIdDecoderMime;
begin
  LDecoder := TIdDecoderMime.Create(nil);
  try
    LDecoder.DecodeStream(AValue, AStream);
  finally
    FreeAndNil(LDecoder);
  end;
end;

class function TEncryption.DecryptStr(const AValue: string; AKey: Word): string;
const
  CKEY1 = 53761;
  CKEY2 = 32618;
var
  i, tmpKey: integer;
  RStr: RawByteString;
  RStrB: TBytes Absolute RStr;
  tmpStr: string;
begin
  tmpStr := UpperCase(AValue);
  SetLength(RStr, Length(tmpStr) div 2);
  i := 1;
  try
    while (i < Length(tmpStr)) do
    begin
      RStrB[i div 2] := StrToInt('$' + tmpStr.ToCharArray[i] +
        tmpStr.ToCharArray[i + 1]);
      Inc(i, 2);
    end;
  except
    Result := '';
    Exit;
  end;
  for i := 0 to Length(RStr) - 1 do
  begin
    tmpKey := RStrB[i];
    RStrB[i] := RStrB[i] xor (AKey shr 8);
    AKey := (tmpKey + AKey) * CKEY1 + CKEY2;
  end;
  Result := UTF8ToString(RStr);
end;

class function TEncryption.Encode64(AValue: string): string;
begin
  var
  LBase64 := TBase64Encoding.Create(0);
  try
    Result := LBase64.Encode(AValue);
  finally
    LBase64.Free;
  end;
  { var
    LEncoder: TIdEncoderMime;
    begin
    LEncoder := TIdEncoderMime.Create(nil);
    try
    Result := LEncoder.EncodeString(AValue);
    finally
    FreeAndNil(LEncoder);
    end; }
end;

class function TEncryption.EncryptStr(const AValue: string;
  AKey: string): string;
var
  i: integer;
  LByte: Byte;
begin
  Result := '';
  for i := 1 to AValue.Length do
  begin
    if Length(AKey) > 0 then
      LByte := Byte(AKey[1 + ((i - 1) mod AKey.Length)]) xor Byte(AValue[i])
    else
      LByte := Byte(AValue[i]);
    Result := Result + IntToHex(LByte, 2).ToLower;
  end;
  { var
    KeyLen, KeyPos, OffSet, SrcPos, SrcAsc, TmpSrcAsc, Range: Integer;
    Dest: String;
    begin
    Result := '';
    if AValue <> '' then
    begin
    Dest := '';
    KeyLen := Length(AKey);
    KeyPos := 0;
    SrcPos := 0;
    SrcAsc := 0;
    Range := 256;

    Randomize;
    OffSet := Random(Range);
    Dest := Format('%1.2x', [OffSet]);
    for SrcPos := 1 to Length(AValue) do
    begin
    SrcAsc := (Ord(AValue.ToCharArray[SrcPos]) + OffSet) Mod 255;  // Src.ToChar[SrcPos]
    if KeyPos < KeyLen then

    KeyPos := KeyPos + 1
    else
    KeyPos := 1;
    SrcAsc := SrcAsc Xor Ord(AKey.ToCharArray[KeyPos]);
    Dest := Dest + Format('%1.2x', [SrcAsc]);
    OffSet := SrcAsc;
    end;

    Result := Dest;
    end; }
end;

class function TEncryption.Encode64(AValue: TStream): string;
var
  LEncoder: TIdEncoderMime;
begin
  LEncoder := TIdEncoderMime.Create(nil);
  try
    Result := LEncoder.EncodeStream(AValue);
  finally
    FreeAndNil(LEncoder);
  end;
end;

class function TEncryption.Encode64FromFile(AFileName: string): string;
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := Encode64(LStream);
  finally
    LStream.Free;
  end;
end;

class function TEncryption.EncryptStr(const AValue: string; AKey: Word): string;
const
  CKEY1 = 53761;
  CKEY2 = 32618;
var
  i: integer;
  RStr: RawByteString;
  RStrB: TBytes Absolute RStr;
begin
  Result := '';
  RStr := UTF8Encode(AValue);
  for i := 0 to Length(RStr) - 1 do
  begin
    RStrB[i] := RStrB[i] xor (AKey shr 8);
    AKey := (RStrB[i] + AKey) * CKEY1 + CKEY2;
  end;
  for i := 0 to Length(RStr) - 1 do
  begin
    Result := Result + IntToHex(RStrB[i], 2);
  end;
end;

class function TEncryption.GetHashSHA2(AValue: string;
  const ASal: string): string;
begin
  Result := THashSHA2.GetHashString(AValue + ASal);
end;

class function TEncryption.GetRandomString(const ALength: integer): String;
const
  aWords = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
begin
  Result := THash.GetRandomString(ALength);

  { repeat
    Result := Result + aWords.ToCharArray[Random(Length(aWords)) + 1];
    until (Length(Result) = aLength); }
end;

class function TEncryption.MD5FromFile(const AValue: string): string;
var
  LIdmd5: TIdHashMessageDigest5;
  LFs: TFileStream;
begin
  Result := EmptyStr;
  LIdmd5 := TIdHashMessageDigest5.Create;
  try
    LFs := TFileStream.Create(AValue, fmOpenRead OR fmShareDenyWrite);
    try
      Result := LIdmd5.HashStreamAsHex(LFs);
    finally
      LFs.Free;
    end;
  finally
    LIdmd5.Free;
  end;
end;

class function TEncryption.MD5FromStream(const AValue: TStream): string;
var
  LIdmd5: TIdHashMessageDigest5;
begin
  Result := EmptyStr;
  LIdmd5 := TIdHashMessageDigest5.Create;
  try
    Result := LIdmd5.HashStreamAsHex(AValue);
  finally
    LIdmd5.Free;
  end;
end;

class function TEncryption.MD5FromString(const AValue: string): string;
var
  LIdmd5: TIdHashMessageDigest5;
begin
  LIdmd5 := TIdHashMessageDigest5.Create;
  try
    Result := LIdmd5.HashStringAsHex(AValue);
  finally
    LIdmd5.DisposeOf;
  end;
end;

class function TEncryption.UrlDecode(const AValue: string): string;
begin
  Result := TNetEncoding.URL.Decode(AValue)
  // TIdURI.UrlDecode(AValue)
    ;
end;

class function TEncryption.UrlEncode(const AValue: string): string;
begin
  Result := TNetEncoding.URL.Encode(AValue)
  // TIdURI.UrlEncode(AValue)
    ;
end;

end.
