# Техническое задание: Модуль визуализации орфографических ошибок

## 1. Общее описание

Модуль предназначен для создания формы визуализации орфографических ошибок, найденных с помощью библиотеки fphunspell в текстовых объектах чертежа ZCAD.

## 2. Существующая система проверки орфографии

### 2.1. Компоненты системы

1. **uzcSpeller.pas** - модуль инициализации и управления проверкой орфографии
   - Содержит глобальную переменную `SpellChecker: TSpeller`
   - Функции `CreateSpellChecker` и `DestroySpellChecker`
   - Загружает словари из пути `ZCSysParams.saved.DictionariesPath`

2. **uzcCommand_SpellCheck.pas** - команда проверки орфографии
   - Регистрирует функцию проверки `SpellCheckString`
   - Использует `SpellChecker.SpellTextSimple()` для проверки текста
   - Интегрируется с командой Find через `RegisterCheckStrProc`

3. **uzcCommand_Find.pas** - инфраструктура поиска
   - Содержит систему регистрации процедур проверки (`TFindProcsRegister`)
   - Типы функций проверки:
     - `TCheckStrA` - проверка AnsiString
     - `TCheckStrU` - проверка UnicodeString
     - `TCheckEnt` - проверка сущностей
   - Функция `CheckEntity` проверяет текстовые объекты (GDBTextID, GDBMTextID)
   - Результаты сохраняются в массиве `Finded: GDBObjOpenArrayOfPV`

### 2.2. Методы TSpeller

- `SpellTextSimple(FindIn: string; var Details: string; Opt: TSpellOpts): TSpellResult`
  - Возвращает результат проверки (например, `WrongLang`)
  - В параметре `Details` возвращает информацию об ошибке/предложение
- `SpellWord(Word: string): TSpellResult` - проверка отдельного слова
- Опции проверки через `TSpellOpts` (например, `CSpellOptDetail`, `CSpellOptFast`)

## 3. Требования к новому модулю

### 3.1. Структура формы

**Файлы модуля:**
- `cad_source/zcad/gui/forms/uzcfspellchecker.pas` - код формы
- `cad_source/zcad/gui/forms/uzcfspellchecker.lfm` - дизайн формы

**Название класса формы:** `TSpellCheckerForm`

### 3.2. Компоненты формы

#### 3.2.1. TToolBar (верхняя панель)
- Название: `ToolBarActions`
- Расположение: `Align = alTop`
- Содержит кнопки управления

#### 3.2.2. TActionList
- Название: `ActionList`
- Действия:
  - `ActionRefresh` - обновить список ошибок (с иконкой обновления)
  - Привязка: `ActionRefresh.OnExecute` -> процедура обновления

#### 3.2.3. Левый TreeView (список ошибок)
- Тип: `TTreeView` или `TListView` (в зависимости от доступности TVisualTreeView)
- Название: `TreeViewErrors`
- Расположение: левая панель, `Align = alLeft` или в `TSplitter`
- Колонки:
  - Ошибочное слово
  - Количество вхождений (опционально)

#### 3.2.4. Правый TreeView (варианты исправления)
- Тип: `TTreeView` или `TListView`
- Название: `TreeViewSuggestions`
- Расположение: правая панель через `TSplitter`
- Отображает варианты исправления для выбранного слова

#### 3.2.5. TLabel (предложение с ошибкой)
- Название: `LabelSentence`
- Расположение: `Align = alBottom`
- Многострочный: `WordWrap = True`, `AutoSize = False`
- Высота: 60-80 пикселей
- Отображает предложение, в котором встретилась ошибка

#### 3.2.6. TSplitter
- Название: `SplitterMain`
- Разделяет левый и правый TreeView

### 3.3. Структура данных

#### 3.3.1. Запись для хранения информации об ошибке

```pascal
TSpellErrorInfo = record
  ErrorWord: string;          // Ошибочное слово
  Sentence: string;           // Предложение с ошибкой
  Suggestions: array of string; // Варианты исправления
  Entity: pGDBObjEntity;      // Указатель на текстовую сущность
end;
```

#### 3.3.2. Массив ошибок

```pascal
TSpellErrorArray = array of TSpellErrorInfo;
```

### 3.4. Основные методы

#### 3.4.1. Инициализация формы

```pascal
procedure TSpellCheckerForm.FormCreate(Sender: TObject);
```
- Инициализация компонентов
- Настройка событий

#### 3.4.2. Поиск орфографических ошибок

```pascal
procedure RefreshSpellErrors;
```
- Вызывает проверку орфографии для всех текстовых объектов чертежа
- Использует инфраструктуру из `uzcCommand_Find.pas`
- Заполняет массив `TSpellErrorArray`
- Обновляет `TreeViewErrors`

Алгоритм:
1. Получить текущий чертеж через `drawings.GetCurrentDWG`
2. Перебрать все текстовые сущности (GDBTextID, GDBMTextID)
3. Для каждого текста вызвать `SpellChecker.SpellTextSimple()`
4. Если найдена ошибка, получить варианты исправления
5. Сохранить информацию в `TSpellErrorArray`

#### 3.4.3. Обработка выбора ошибки

```pascal
procedure TreeViewErrorsSelectionChanged(Sender: TObject);
```
- Получает выбранную ошибку из `TreeViewErrors`
- Заполняет `TreeViewSuggestions` вариантами исправления
- Обновляет `LabelSentence` предложением с ошибкой

#### 3.4.4. Получение вариантов исправления

```pascal
function GetSuggestions(const Word: string): TStringArray;
```
- Использует методы TSpeller для получения вариантов исправления
- Возвращает массив строк с вариантами

### 3.5. Интеграция с системой

#### 3.5.1. Регистрация формы

Форма должна быть доступна через команду или меню ZCAD:

```pascal
procedure ShowSpellCheckerForm;
begin
  if not Assigned(SpellCheckerForm) then
    SpellCheckerForm := TSpellCheckerForm.Create(Application);
  SpellCheckerForm.Show;
  SpellCheckerForm.RefreshSpellErrors;
end;
```

#### 3.5.2. Регистрация команды (опционально)

В файле инициализации можно зарегистрировать команду:

```pascal
CreateZCADCommand(@ShowSpellChecker_com, 'SpellCheckList', 0, 0);
```

## 4. Требования к коду

### 4.1. Стандарты кодирования (согласно CLAUDE.md)

1. **Модульность:**
   - Модуль не должен превышать 300-500 строк
   - Каждая функция выполняет одно действие, не более 30 строк
   - Вложенность не более 3 уровней

2. **Именование:**
   - Осмысленные имена переменных и функций
   - Без сокращений и аббревиатур

3. **Комментарии:**
   - Все комментарии на русском языке
   - Комментарии перед каждой функцией
   - Комментарии для сложных участков кода

4. **Форматирование:**
   - Единообразные отступы (2 или 4 пробела)
   - Максимальная длина строки - 100 символов
   - Одна пустая строка между логическими блоками

5. **Заголовок файла:**

```pascal
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
```

6. **Логирование:**
   - Использовать `uzclog`
   - Тип сообщений: `LM_Info`
   - Пример: `programlog.LogOutFormatStr('uzcfspellchecker: errors found = %d', [ErrorCount], LM_Info);`

### 4.2. Зависимости (uses)

```pascal
uses
  // Системные модули
  SysUtils, Classes, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  ActnList, ToolWin,
  // ZCAD модули
  uzcLog,
  uzcSpeller, uSpeller,
  uzcdrawings,
  uzeentity, uzeenttext,
  uzccommandsabstract, uzccommandsimpl;
```

## 5. Этапы реализации

1. Создание файла формы `.lfm` с базовой разметкой
2. Создание файла `.pas` с объявлением класса
3. Реализация метода `RefreshSpellErrors`
4. Реализация обработчиков событий для TreeView
5. Добавление метода получения вариантов исправления
6. Интеграция с командной системой ZCAD
7. Тестирование на реальных данных
8. Документирование кода

## 6. Дополнительные функции (опционально, для будущих версий)

- Двойной клик на варианте исправления - применить исправление
- Кнопка "Игнорировать" - пропустить ошибку
- Кнопка "Добавить в словарь" - добавить слово в пользовательский словарь
- Фильтрация ошибок по типу
- Экспорт списка ошибок в файл

## 7. Примечания

- TVisualTreeView может быть не доступен напрямую, использовать стандартный TTreeView или TListView
- Для получения списка вариантов исправления может потребоваться доработка TSpeller
- Необходимо обеспечить корректную работу с Unicode строками
