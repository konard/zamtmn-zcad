{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}
{$mode objfpc}{$H+}
{$INCLUDE zengineconfig.inc}

unit uzcfspellchecker;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ActnList, ToolWin, Buttons,
  uzcLog,
  uzcSpeller, uSpeller,
  uzcdrawings,
  uzeentity,
  uzeenttext,
  uzeconsts,
  gzctnrVectorTypes;

type
  // Запись для хранения информации об одной орфографической ошибке
  TSpellErrorInfo = record
    ErrorWord: string;           // Ошибочное слово
    Sentence: string;            // Предложение, в котором встретилась ошибка
    Suggestions: TStringArray;   // Варианты исправления
    Entity: pGDBObjEntity;       // Указатель на текстовую сущность
  end;

  TSpellErrorArray = array of TSpellErrorInfo;

  { TSpellCheckerForm }

  TSpellCheckerForm = class(TForm)
    ActionRefresh: TAction;
    ActionListMain: TActionList;
    ImageListActions: TImageList;
    LabelErrors: TLabel;
    LabelSuggestions: TLabel;
    LabelSentence: TLabel;
    LabelSentenceCaption: TLabel;
    ListViewErrors: TListView;
    ListViewSuggestions: TListView;
    PanelBottom: TPanel;
    PanelErrors: TPanel;
    PanelMain: TPanel;
    PanelSuggestions: TPanel;
    SplitterMain: TSplitter;
    ToolBarActions: TToolBar;
    ToolButtonRefresh: TToolButton;
    procedure ActionRefreshExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewErrorsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FErrorsArray: TSpellErrorArray;
    procedure ClearErrorsArray;
    procedure RefreshSpellErrors;
    procedure UpdateErrorsList;
    procedure UpdateSuggestionsList(AIndex: Integer);
    procedure UpdateSentenceLabel(AIndex: Integer);
    procedure ScanDrawingForErrors;
    procedure ProcessTextEntity(AEntity: pGDBObjEntity);
    procedure AddError(const AWord, ASentence: string; AEntity: pGDBObjEntity);
  public
    { public declarations }
  end;

var
  SpellCheckerForm: TSpellCheckerForm;

procedure ShowSpellCheckerForm;

implementation

{$R *.lfm}

{ TSpellCheckerForm }

// Инициализация формы при создании
procedure TSpellCheckerForm.FormCreate(Sender: TObject);
begin
  programlog.LogOutStr('uzcfspellchecker: form created', LM_Info);
  SetLength(FErrorsArray, 0);
  LabelSentence.Caption := '';
end;

// Очистка ресурсов при закрытии формы
procedure TSpellCheckerForm.FormDestroy(Sender: TObject);
begin
  ClearErrorsArray;
  programlog.LogOutStr('uzcfspellchecker: form destroyed', LM_Info);
end;

// Обработчик нажатия кнопки "Обновить"
procedure TSpellCheckerForm.ActionRefreshExecute(Sender: TObject);
begin
  RefreshSpellErrors;
end;

// Обработчик выбора элемента в списке ошибок
procedure TSpellCheckerForm.ListViewErrorsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  selectedIndex: Integer;
begin
  if not Selected then
    Exit;

  if Item = nil then
    Exit;

  selectedIndex := Item.Index;
  if (selectedIndex >= 0) and (selectedIndex < Length(FErrorsArray)) then
  begin
    UpdateSuggestionsList(selectedIndex);
    UpdateSentenceLabel(selectedIndex);
  end;
end;

// Очистка массива ошибок
procedure TSpellCheckerForm.ClearErrorsArray;
var
  i: Integer;
begin
  for i := 0 to High(FErrorsArray) do
  begin
    SetLength(FErrorsArray[i].Suggestions, 0);
  end;
  SetLength(FErrorsArray, 0);
end;

// Основная процедура обновления списка орфографических ошибок
procedure TSpellCheckerForm.RefreshSpellErrors;
begin
  programlog.LogOutStr('uzcfspellchecker: refreshing spell errors', LM_Info);

  ClearErrorsArray;
  ListViewErrors.Items.Clear;
  ListViewSuggestions.Items.Clear;
  LabelSentence.Caption := '';

  if drawings.GetCurrentDWG = nil then
  begin
    programlog.LogOutStr('uzcfspellchecker: no current drawing', LM_Info);
    Exit;
  end;

  ScanDrawingForErrors;
  UpdateErrorsList;

  programlog.LogOutFormatStr('uzcfspellchecker: found %d errors',
    [Length(FErrorsArray)], LM_Info);
end;

// Обновление отображения списка ошибок в ListView
procedure TSpellCheckerForm.UpdateErrorsList;
var
  i: Integer;
  listItem: TListItem;
begin
  ListViewErrors.Items.BeginUpdate;
  try
    ListViewErrors.Items.Clear;
    for i := 0 to High(FErrorsArray) do
    begin
      listItem := ListViewErrors.Items.Add;
      listItem.Caption := FErrorsArray[i].ErrorWord;
    end;
  finally
    ListViewErrors.Items.EndUpdate;
  end;
end;

// Обновление списка вариантов исправления для выбранной ошибки
procedure TSpellCheckerForm.UpdateSuggestionsList(AIndex: Integer);
var
  i: Integer;
  listItem: TListItem;
begin
  ListViewSuggestions.Items.BeginUpdate;
  try
    ListViewSuggestions.Items.Clear;

    if (AIndex < 0) or (AIndex >= Length(FErrorsArray)) then
      Exit;

    for i := 0 to High(FErrorsArray[AIndex].Suggestions) do
    begin
      listItem := ListViewSuggestions.Items.Add;
      listItem.Caption := FErrorsArray[AIndex].Suggestions[i];
    end;
  finally
    ListViewSuggestions.Items.EndUpdate;
  end;
end;

// Обновление метки с предложением, содержащим ошибку
procedure TSpellCheckerForm.UpdateSentenceLabel(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FErrorsArray)) then
  begin
    LabelSentence.Caption := '';
    Exit;
  end;

  LabelSentence.Caption := FErrorsArray[AIndex].Sentence;
end;

// Сканирование чертежа на наличие орфографических ошибок
procedure TSpellCheckerForm.ScanDrawingForErrors;
var
  currentDrawing: PTSimpleDrawing;
  pEntity: pGDBObjEntity;
  ir: itrec;
begin
  currentDrawing := drawings.GetCurrentDWG;
  if currentDrawing = nil then
    Exit;

  // Перебираем все объекты в чертеже
  pEntity := currentDrawing^.GetCurrentROOT^.ObjArray.beginiterate(ir);
  if pEntity <> nil then
  repeat
    ProcessTextEntity(pEntity);
    pEntity := currentDrawing^.GetCurrentROOT^.ObjArray.iterate(ir);
  until pEntity = nil;
end;

// Обработка одной текстовой сущности на наличие орфографических ошибок
procedure TSpellCheckerForm.ProcessTextEntity(AEntity: pGDBObjEntity);
var
  entityType: TObjID;
  textContent: string;
  textEntity: PGDBObjText;
  errorWord: string;
  spellResult: Integer;
  suggestions: TStringList;
  i: Integer;
begin
  if AEntity = nil then
    Exit;

  entityType := AEntity^.GetObjType;

  // Проверяем только текстовые объекты
  if (entityType <> GDBTextID) and (entityType <> GDBMTextID) then
    Exit;

  textEntity := PGDBObjText(AEntity);
  textContent := string(textEntity^.Content);

  if textContent = '' then
    Exit;

  // Проверяем текст на орфографические ошибки
  errorWord := '';
  try
    // SpellTextSimple возвращает:
    // - положительное число (TLangHandle) если текст корректный
    // - WrongLang (-1) если найдена ошибка
    // - MixedLang (-2) если смешанные языки
    // - NoText (-3) если нет текста для проверки
    spellResult := SpellChecker.SpellTextSimple(
      textContent,
      errorWord,
      TSpeller.CSpellOptDetail
    );

    // Если найдена ошибка
    if spellResult = TSpeller.WrongLang then
    begin
      if errorWord <> '' then
      begin
        // Получаем варианты исправления
        suggestions := TStringList.Create;
        try
          SpellChecker.Suggest(errorWord, suggestions);

          // Добавляем ошибку в массив
          AddError(errorWord, textContent, AEntity);

          // Копируем варианты исправления
          if suggestions.Count > 0 then
          begin
            SetLength(
              FErrorsArray[High(FErrorsArray)].Suggestions,
              suggestions.Count
            );
            for i := 0 to suggestions.Count - 1 do
            begin
              FErrorsArray[High(FErrorsArray)].Suggestions[i] :=
                suggestions[i];
            end;
          end;
        finally
          suggestions.Free;
        end;
      end;
    end;
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzcfspellchecker: error checking text: %s',
        [E.Message], LM_Info);
  end;
end;

// Добавление ошибки в массив
procedure TSpellCheckerForm.AddError(const AWord, ASentence: string;
  AEntity: pGDBObjEntity);
var
  errorIndex: Integer;
begin
  errorIndex := Length(FErrorsArray);
  SetLength(FErrorsArray, errorIndex + 1);

  FErrorsArray[errorIndex].ErrorWord := AWord;
  FErrorsArray[errorIndex].Sentence := ASentence;
  FErrorsArray[errorIndex].Entity := AEntity;

  // Инициализируем пустой массив вариантов исправления
  // В реальной реализации здесь должен быть вызов метода
  // получения вариантов из TSpeller
  SetLength(FErrorsArray[errorIndex].Suggestions, 0);
end;

// Процедура отображения формы проверки орфографии
procedure ShowSpellCheckerForm;
begin
  if not Assigned(SpellCheckerForm) then
    SpellCheckerForm := TSpellCheckerForm.Create(Application);

  SpellCheckerForm.Show;
  SpellCheckerForm.RefreshSpellErrors;
end;

end.
