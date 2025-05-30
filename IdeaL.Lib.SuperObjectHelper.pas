unit IdeaL.Lib.SuperObjectHelper;

interface

uses
  XSuperObject,

{$IF defined(FMX)}
  FMX.Edit,
  FMX.Memo,
  FMX.Controls
{$ELSE}
  Vcl.StdCtrls,
  Vcl.Controls
{$IFEND}
    ,
  System.Generics.Collections,
  System.SysUtils,
  System.StrUtils,
  System.Variants
    ;

type
  ISuperObject = XSuperObject.ISuperObject;
  ISuperArray = XSuperObject.ISuperArray;
  IMember = XSuperObject.IMember;
  ICast = XSuperObject.ICast;

  TSuperObjectHelper = class
  private
    { Private declarations }
  public
    class function SA(AJson: string = ''): ISuperArray;
    class function SO(const AJson: string = ''): ISuperObject;
    class procedure DataToScreen(ASObj: ISuperObject; ADic: TDictionary<TControl, string>);
    class function DataFromScreen(ASObjOrigem: ISuperObject; ADic: TDictionary<TControl, string>): ISuperObject;
    class function SuperArrayGetItem(ASAry: ISuperArray; const AIndex: Integer): ISuperObject;
    class function SuperIsFieldAssigned(ASObj: ISuperObject; AField: string): Boolean;
    { Public declarations }
  protected
    { protected declarations }
  published
    { Published declarations }
  end;

implementation

uses
  IdeaL.Lib.Utils, 
  FMX.StdCtrls;

{ TSuperObjectHelper }

class function TSuperObjectHelper.DataFromScreen(ASObjOrigem: ISuperObject;
  ADic: TDictionary<TControl, string>): ISuperObject;
var
  LKey: TControl;
  LType: string;
  LSValue: string;
  LText: string;
  LInteger: Integer;
  LDateTime: TDateTime;
begin
  Result := SO(EmptyStr);

  if not Assigned(ASObjOrigem) or not(Assigned(ADic)) then
    Exit;

  for LKey in ADic.Keys do
  begin
    if (Assigned(LKey)) and
      not(LKey.Hint.Trim.IsEmpty) and
      (ADic.TryGetValue(LKey, LType))
    then
    begin
      LText := EmptyStr;
      LSValue := VarToStr(ASObjOrigem.V[LKey.Hint.Trim]);

      if LKey.InheritsFrom(TEdit) then
        LText := TEdit(LKey).Text
      else if LKey.InheritsFrom(TCustomMemo) then
        LText := TCustomMemo(LKey).Lines.Text
      else if LKey.InheritsFrom(TLabel) then
        LText := TLabel(LKey).Text;

      if not LSValue.Equals(LText) then
      begin
        case AnsiIndexStr(UpperCase(LType), ['D', 'I', 'S']) of
          0:
            begin
              if (TryStrToDateTime(LText, LDateTime)) then
                Result.D[LKey.Hint.Trim] := LDateTime;
            end;
          1:
            begin
              if (TryStrToInt(LText, LInteger)) then
              begin
                Result.I[LKey.Hint.Trim] := LText.ToInteger
              end;
            end;
          2:
            begin
              Result.S[LKey.Hint.Trim] := LText;
            end;
        end;
      end;
    end;
  end;
end;

class procedure TSuperObjectHelper.DataToScreen(ASObj: ISuperObject;
  ADic: TDictionary<TControl, string>);
  procedure SetText(AKey: TControl; AValue: string);
  begin
    if AKey.InheritsFrom(TEdit) then
      TEdit(AKey).Text := AValue
    else if AKey.InheritsFrom(TLabel) then
      TLabel(AKey).Text := AValue
    else if AKey.InheritsFrom(TCustomMemo) then
      TCustomMemo(AKey).Lines.Text := AValue;
  end;
var
  LKey: TControl;
  LType: string;
  LSValue: string;
  LDateTime: TDateTime;
begin
  if not Assigned(ASObj) or not(Assigned(ADic)) then
    Exit;

  for LKey in ADic.Keys do
  begin
    if (Assigned(LKey)) and not(LKey.Hint.Trim.IsEmpty) and
      (SuperIsFieldAssigned(ASObj, LKey.Hint.Trim)) and
      (ADic.TryGetValue(LKey, LType))
    then
    begin
      LSValue := VarToStr(ASObj.V[LKey.Hint.Trim]);
      case AnsiIndexStr(UpperCase(LType), ['D', 'I', 'S']) of
        0:
          begin
            if TryStrToDate(LSValue, LDateTime) then
              SetText(LKey, FormatDateTime(FormatSettings.ShortDateFormat, LDateTime))
            else
              SetText(LKey, EmptyStr);
          end;
        1, 2:
          begin
            SetText(LKey, LSValue);
          end;
      else
        SetText(LKey, EmptyStr);
      end;
    end;
  end;
end;

class function TSuperObjectHelper.SA(AJson: string): ISuperArray;
begin
  if not AJson.StartsWith('[') then // force it to become an array
    AJson := '[' + AJson + ']';
  Result := TSuperObjectHelper.SO(AJson).AsArray;
end;

class function TSuperObjectHelper.SO(const AJson: string): ISuperObject;
begin
  Result := nil;
  try
    Result := XSuperObject.SO(AJson);
  except
    TUtils.ThrowExceptionInvalidJsonValue(AJson);
  end;
end;

class function TSuperObjectHelper.SuperArrayGetItem(ASAry: ISuperArray;
  const AIndex: Integer): ISuperObject;
var
  LCast: ICast;
  I: Integer;
begin
  Result := nil;
  I := 0;
  if (AIndex < 0) or (AIndex > Pred(ASAry.Length)) then
    raise Exception.Create('List index out of bounds');
  for LCast in ASAry do
  begin
    if I = AIndex then
    begin
      Result := LCast.GetObject;
      Break;
    end;
    Inc(I, 1);
  end;
end;

class function TSuperObjectHelper.SuperIsFieldAssigned(ASObj: ISuperObject;
  AField: string): Boolean;
begin
  Result := ASObj.Null[AField] = TMemberStatus.jAssigned;
end;

end.
