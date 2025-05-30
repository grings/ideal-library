unit IdeaL.Lib.JSONManage;

interface

uses
  System.SysUtils,

  Data.DB,

  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,

  uDWJSONObject,
  uDWConstsData,
  uDWConsts, uDWConstsCharset;

type
TJSONManage = class
  public
    class function DataSetToJSONValue(const aFdDataSet: TFDDataSet; const aTable: String = 'temp'): String;
    class function JSONValueToDataSet1(const aJsonValue: String): TFDDataSet;
  private
end;

implementation

{ TJSONManage }

class function TJSONManage.DataSetToJSONValue(const aFdDataSet: TFDDataSet; const aTable: String = 'temp'): String;
var
  vJSONValue: TJSONValue;
begin
  try
    Result := '';

    vJSONValue := TJSONValue.Create;
    vJSONValue.Encoding := TEncodeSelect.esUtf8;
    vJSONValue.LoadFromDataset(aTable, aFdDataSet, True);
    Result := vJSONValue.ToJSON;
  finally
    FreeAndNil(vJSONValue);
  end;
end;

class function TJSONManage.JSONValueToDataSet1(
  const aJsonValue: String): TFDDataSet;
var
  vJSONValue: TJSONValue;
begin
  try
    Result := TFDMemTable.Create(nil);

    vJSONValue := TJSONValue.Create;
    vJSONValue.Encoding := TEncodeSelect.esUtf8;
    vJSONValue.WriteToDataset(dtFull, aJsonValue, Result);
  finally
    //FreeAndNil(Result);
    FreeAndNil(vJSONValue);
  end;
end;

end.
