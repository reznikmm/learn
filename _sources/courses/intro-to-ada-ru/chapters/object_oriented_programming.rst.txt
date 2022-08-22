Объектно-ориентированное программирование
=========================================

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Объектно-ориентированное программирование (ООП) - это большая и
расплывчатая концепция в языках программирования, которая имеет
тенденцию включать в себя множество различных элементов, потому, что разные
языки часто реализуют свое собственное видение этой концепции,
предлагая в чем-то сходные, а в чем-то отличающиеся реализации.

Но одна из моделей, можно сказать, «выиграла» битву за звания "истинного"
объектно-ориентированного подхода, хотя бы, если судить только по
популярности. Это модель, используется в языке программирования Java,
и она очень похожа на модель, используемую в C ++. Вот некоторые
важные характеристики:

-  Производные типы и расширение типов: Большинство объектно-ориентированных
   языков позволяют пользователю добавлять новые поля в производные типы.

-  Заменяемость подтипов: Объекты типа, производного от базового типа, в
   некоторых случаях, могут использоваться вместо объектов базового типа.

-  Полиморфизм среды выполнения: Вызов подпрограммы, обычно называемой
   *методом*, привязанной к типу объекта, может диспечеризироваться во
   время выполнения программы в зависимости от конкретного типа объекта.

-  Инкапсуляция: Объекты могут скрывать некоторые свои данные.

-  Расширяемость: пользователи "извне" вашего пакета или даже всей вашей
   библиотеки могут создавать производные от ваших объектных типов
   и определять их поведение по своему.

Ада появилась еще до того, как объектно-ориентированное
программирование стало таким уж популярным, как и сегодня. Некоторые
механизмы и концепции из приведенного выше списка были в самой ранней
версии Ада еще до того, как было добавлено то, что мы бы назвали
поддержкой ООП:

-  Как мы видели, инкапсуляция реализована в Аде не на уровне типа, а на
   уровне пакета.

-  Заменяемость подтипов может быть реализована с использованием, ну да,
   подтипов, которые имеют полную и "разрешительную" (permissive)
   статическую модель замещаемости.
   Во время выполнения замена завершится неудачно, если динамические
   ограничения подтипа будут нарушены.

-  Полиморфизм времени выполнения может быть реализован с использованием
   записей с вариантами.

Однако в этом списке нет расширения типов, если вы не
считать записи с вариантами, и расширяемости.

Редакция Ада 1995 года добавила функцию, заполняющую пробелы, которая
позволила людям проще программировать, следуя объектно-ориентированной
парадигме. Эта функция называется *теговые типы*.

.. note::
    Примечание: В Ада можно написать программу не создав даже одного
    тегового типа. Если вы предпочитаете такой стиль программирования или
    вам в данный момент не нужны теговые типы, это нормально не
    использовать их, как в случае и со многими другими возможностями Ады.

    Тем не менее, может оказаться, что они - наилучший способ выразить
    решение вашей задачи. А, раз это так, читайте дальше!

Производные типы
----------------

Прежде чем представить теговые типы, мы должны обсудить тему,
которой мы уже касались, но на самом деле не углублялись в нее до сих пор:

Вы можете создать один или несколько новых типов из любого типа языка
Ада. Производные типы встроены в язык.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Newtypes

    package Newtypes is
       type Point is record
           X, Y : Integer;
       end record;

       type New_Point is new Point;
    end Newtypes;

Наследование типов полезно для обеспечения строгой типизации,
поскольку система типов рассматривает эти два типа как несовместимые.

Но этим дело не ограничивается: создавая производный тип вы наследуете
от него многое. Вы наследуете не только
представление данных, но также можете унаследовать и поведение.

Когда вы наследуете тип, вы также наследуете так называемые
примитивные операции. *Примитивная операция* (или просто *примитив*) -
это подпрограмма, привязанная к типу. Ада определяет примитивы как
подпрограммы, определенные в той же области, что и тип.

.. attention::
    Обратите внимание: подпрограмма станет примитивом этого типа только в
    том случае, если:

    1. Подпрограмма объявляется в той же области, что и тип и
    2. Тип и подпрограмма объявляются в пакете.

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Primitives

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Primitives is
      package Week is
        type Days is (Monday, Tuesday, Wednesday,
                      Thursday, Friday,
                      Saturday, Sunday);

         --  Print_Day is a primitive
         --  of the type Days
        procedure Print_Day (D : Days);
      end Week;

      package body Week is
        procedure Print_Day (D : Days) is
        begin
           Put_Line (Days'Image (D));
        end Print_Day;
      end Week;

      use Week;
      type Weekend_Days is new
        Days range Saturday .. Sunday;

      --  A procedure Print_Day is automatically
      --  inherited here. It is as if the procedure
      --
      --  procedure Print_Day (D : Weekend_Days);
      --
      --  has been declared with the same body

      Sat : Weekend_Days := Saturday;
    begin
       Print_Day (Sat);
    end Primitives;

Этот вид наследования может быть очень полезным и не ограничивается
типами записей (вы можете использовать его и для дискретных типов, как в
примере выше), но он лишь внешне похож на объектно-ориентированное
наследование:

-  Записи не могут быть расширены с помощью этого механизма. Вы
   также не можете указать новое представление для нового типа: оно
   **всегда** будет то же, что и у базового типа.
   (*Прим. пер:* На самом деле, это не так и производные типы часто
   используются чтобы выполнять преобразование внутреннего
   представления типа.)

-  Нет возможности для динамической диспетчеризации или полиморфизма.
   Объекты имеют фиксированный, статический тип.

Есть и другие различия, но перечислять их все здесь нет смысла. Просто
помните, что эту форму наследования вы можете использовать, если хотите
иметь только статически унаследованое поведение,
избежать дублирования кода и использования композиции, и которое вам не
подходит, если вам нужны какие-либо динамические возможности,
которые обычно ассоциируются с ООП.

Теговые типы
------------

Версия языка Ада 1995 года представила теговые типы, чтобы
удовлетворить потребность в едином решении, которое позволяет
программировать в объектно-ориентированном стиле, аналогичном тому,
что был описан в начале этой главы.

Теговые типы очень похожи на обычные записи, за исключением того, что
добавлена следующая функциональность:

-  Типы имеют *тег*, хранящийся внутри каждого объекта и необходимый чтобы
   определить тип объекта
   `во время выполнения <https://ru.wikipedia.org/wiki/Динамическая_идентификация_типа_данных>`_.

-  Для примитивов может приминяться диспечеризация. Примитив тегового типа -
   это то, что вы бы назвали *методом* в Java или C++. Если у вас есть тип,
   производный от базового типа с переопределенным примитивом,
   то при вызове примитива для объекта, какой примитив вызовется будет
   зависить от точного типа объекта в момент исполнения.

-  Введены специальные правила, позволяющие теговому типу,
   производному от базового типа, быть статически совместимым с базовым
   типом.

Давайте посмотрим на наши первые объявления тегового типа:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    package P is
       type My_Class is tagged null record;
       --  Just like a regular record, but
       --  with tagged qualifier

       --  Methods are outside of the type
       --  definition:

       procedure Foo (Self : in out My_Class);
       --  If you define a procedure taking a
       --  My_Class argument in the same package,
       --  it will be a method.

       --  Here's how you derive a tagged type:

       type Derived is new My_Class with record
           A : Integer;
           --  You can add fields in derived types.
       end record;

       overriding procedure Foo (Self : in out Derived);
       --  The "overriding" qualifier is optional,
       --  but if it is present, it must be valid.
    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is
       procedure Foo (Self : in out My_Class) is
       begin
          Put_Line ("In My_Class.Foo");
       end Foo;

       procedure Foo (Self : in out Derived) is
       begin
          Put_Line ("In Derived.Foo, A = "
                    & Integer'Image (Self.A));
       end Foo;
    end P;

Надклассовые типы
-----------------

Чтобы сохранить согласованность языка, необходимо
было ввести новую нотацию, обозначающую: "Данный объект относится к
этому теговому типу или любому его потомку".

В Аде мы называем это *надклассовым типом*. Он используется в
ООП, как только вам понадобится полиморфизм. Например, вы не можете
выполнить следующие действия:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types
    :class: ada-expect-compile-error

    with P; use P;

    procedure Main is

       O1 : My_Class;
       --  Declaring an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declaring an object of type Derived

       O3 : My_Class := O2;
       --  INVALID: Trying to assign a value
       --  of type derived to a variable of
       --  type My_Class.
    begin
       null;
    end Main;

Это связано с тем, что объект типа :ada:`T` имеет в точности тип :ada:`T`,
независимо от того, является :ada:`T` теговым или нет. То, что
программист пытается тут сказать, это «Я хочу, чтобы O3 содержал объект
типа :ada:`My_Class` или любого производного от :ada:`My_Class` типа».
Вот как вы это делаете:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declare an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declare an object of type Derived

       O3 : My_Class'Class := O2;
       --  Now valid: My_Class'Class designates
       --  the classwide type for My_Class,
       --  which is the set of all types
       --  descending from My_Class (including
       --  My_Class).
    begin
       null;
    end Main;

.. attention::
    Обратите внимание: Поскольку объект надклассового типа может быть размером
    с любого потомка его базового типа, то его размер заранее неизвестен. Таким
    образом, это неопределенный тип со всеми ожидаемыми ограничениями:

    - Он не может быть использован для поля/компоненты записи

    - Объект надклассового типа должен быть инициализирован немедленно (вы не
      можете ограничить такой тип каким-либо иным способом, кроме
      как путем его инициализации).

Операции диспетчеризации
------------------------

Мы увидели, что можно переопределять операции в типах, производных от
другого тегового типа. Конечной целью ООП является выполнение
диспетчеризируемого вызова: когда вызоваемый примитив (метод) определяется
точным типом объекта.

Но если задуматься, переменная типа :ada:`My_Class` всегда содержит объект
именно данного типа. Если требуется переменная, которая может содержать
:ada:`My_Class` или любой производный тип, она должна иметь тип
:ada:`My_Class'Class`.

Другими словами, чтобы сделать диспетчеризируемый вызов, вы должны
сначала получить объект, который может иметь либо конкретный тип, либо
любой тип, производным от этого конкретного типа, а именно объект
надклассового типа.

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declare an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declare an object of type Derived

       O3 : My_Class'Class := O2;

       O4 : My_Class'Class := O1;
    begin
       Foo (O1);
       --  Non dispatching: Calls My_Class.Foo
       Foo (O2);
       --  Non dispatching: Calls Derived.Foo
       Foo (O3);
       --  Dispatching: Calls Derived.Foo
       Foo (O4);
       --  Dispatching: Calls My_Class.Foo
    end Main;

.. admonition:: Внимание

    Вы можете преобразовать объект типа :ada:`Derived` в объект типа
    :ada:`My_Class`. В Аде это называется
    *преобразованием представления* и полезно, например, если
    вы хотите вызвать родительский метод.

    В том случае, когда объект действительно преобразуется в объект
    :ada:`My_Class`, что включает изменение его тега. Поскольку теговые объекты всегда
    передаются по ссылке, вы можете использовать этот вид преобразования
    для изменения состояния объекта: изменения в преобразованном объекте
    повлияют на оригинал. (*Прим. пер.:* Это не так, только преобразование
    представление позволяет менять оригинал.)

    .. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

        with P; use P;

        procedure Main is
           O1 : Derived := (A => 12);
           --  Declare an object of type Derived

           O2 : My_Class := My_Class (O1);

           O3 : My_Class'Class := O2;
        begin
           Foo (O1);
           --  Non dispatching: Calls Derived.Foo
           Foo (O2);
           --  Non dispatching: Calls My_Class.Foo

           Foo (O3);
           --  Dispatching: Calls My_Class.Foo
        end Main;

Точечная нотация
----------------

Вы также можете вызывать примитивы теговых типов с помощью
нотации, более привычной объектно-ориентированным программистам.
Учитывая приведенный выше примитив Foo, вы также можете написать
указанную выше программу следующим образом:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    with P; use P;

    procedure Main is
       O1 : My_Class;
       --  Declare an object of type My_Class

       O2 : Derived := (A => 12);
       --  Declare an object of type Derived

       O3 : My_Class'Class := O2;

       O4 : My_Class'Class := O1;
    begin
       O1.Foo;
       --  Non dispatching: Calls My_Class.Foo
       O2.Foo;
       --  Non dispatching: Calls Derived.Foo
       O3.Foo;
       --  Dispatching: Calls Derived.Foo
       O4.Foo;
       --  Dispatching: Calls My_Class.Foo
    end Main;

Если диспетчерезирующий параметр примитива является первым параметром, как
в наших примерах, вы можете вызвать примитив, используя точечную
нотацию. Все оставшиеся параметры передаются обычным образом:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Types

    with P; use P;

    procedure Main is
       package Extend is
          type D2 is new Derived with null record;

          procedure Bar (Self : in out D2;
                         Val  :        Integer);
       end Extend;

       package body Extend is
          procedure Bar (Self : in out D2;
                         Val  :        Integer) is
          begin
             Self.A := Self.A + Val;
          end Bar;
       end Extend;

       use Extend;

       Obj : D2 := (A => 15);
    begin
       Obj.Bar (2);
       Obj.Foo;
    end Main;

Личные и лимитируемые типы с тегами
------------------------------------

Ранее мы видели (в главе :doc:`./privacy`), что типы могут быть
объявлены лимитируемыми или личными. Эти методы инкапсуляции также
могут применяться к теговым типам, как мы увидим в этом разделе.

Это пример личного тегового типа:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Private_Types

    package P is
       type T is tagged private;
    private
       type T is tagged record
           E : Integer;
       end record;
    end P;

Это пример лимитируемого тегового типа:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Limited_Types

    package P is
       type T is tagged limited record
           E : Integer;
       end record;
    end P;

Естественно, вы можете комбинировать как *лимитируемые*, так и *личные*
типы и объявить лимитируемый личный теговый тип:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Tagged_Limited_Private_Types

    package P is
       type T is tagged limited private;

       procedure Init (A : in out T);
    private
       type T is tagged limited record
           E : Integer;
       end record;
    end P;

    package body P is

       procedure Init (A : in out T) is
       begin
          A.E := 0;
       end Init;

    end P;

    with P; use P;

    procedure Main is
      T1, T2 : T;
    begin
      T1.Init;
      T2.Init;

      --  The following line doesn't work
      --  because type T is private:
      --
      --  T1.E := 0;

      --  The following line doesn't work
      --  because type T is limited:
      --
      --  T2 := T1;
    end Main;

Обратите внимание, что код в процедуре :ada:`Main` имеет два
оператора присваивания,
которые вызывают ошибки компиляции, потому что тип :ada:`T` является
лимитируемым личным. Фактически, вы не можете:

-  присваивать :ada:`T1.E` непосредственно, потому что тип :ada:`T`
   является личным;

-  присваивать :ada:`T1` в :ada:`T2`, потому что тип :ada:`T` ограничен.

В этом случае нет различия между теговими типами и типами без тегов: эти
ошибки компиляции также могут возникать и для нетеговых типов.

Надклассовые ссылочные типы
---------------------------

В этом разделе мы обсудим полезный шаблон для
объектно-ориентированного программирования в Аде: надклассовые ссылочные типы.
Начнем с примера, в котором мы объявляем теговый тип :ada:`T` и
производный тип :ada:`T_New`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Error

    package P is
       type T is tagged null record;

       procedure Show (Dummy : T);

       type T_New is new T with null record;

       procedure Show (Dummy : T_New);
    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Show (Dummy : T) is
       begin
          Put_Line ("Using type "
                    & T'External_Tag);
       end Show;

       procedure Show (Dummy : T_New) is
       begin
          Put_Line ("Using type "
                    & T_New'External_Tag);
       end Show;

    end P;

Обратите внимание, как мы используем пустые записи для типов :ada:`T` и
:ada:`T_New`. Хотя эти типы на самом деле не имеют каких-либо компонент,
мы по-прежнему можем использовать их для демонстрации диспетчеризации.
Также обратите внимание, что в приведенном выше примере используется
атрибут :ada:`'External_Tag` в реализации процедуры :ada:`Show` для
получения строки с названием соответствующего тегового типа.

Как мы видели ранее, мы должны использовать надклассовый тип для
создания объектов, которые могут выполнять диспетчерезируемые вызовы.
Другими словами, будут диспетчеризироваться объекты типа :ada:`T'Class`.
Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Error

    with P; use P;

    procedure Dispatching_Example is
      T2         :          T_New;
      T_Dispatch : constant T'Class := T2;
    begin
      T_Dispatch.Show;
    end Dispatching_Example;

Более полезным приложением является объявление массива объектов,
для которых будет выполняться диспетчеризация. Например, мы хотели бы
объявить массив :ada:`T_Arr`, перебрать в цикле этот массив и выполнить
диспетчеризацию в соответствии с фактическим типом каждого отдельного
элемента массива:

.. code-block:: ada

    for I in T_Arr'Range loop
       T_Arr (I).Show;
       --  Call Show procedure according
       --  to actual type of T_Arr (I)
    end loop;

Однако непосредственно объявить массив с элементами :ada:`T'Class` невозможно:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Error
    :class: ada-expect-compile-error

    with P; use P;

    procedure Classwide_Compilation_Error is
      T_Arr  : array (1 .. 2) of T'Class;
      --                         ^
      --               Compilation Error!
    begin
      for I in T_Arr'Range loop
         T_Arr (I).Show;
      end loop;
    end Classwide_Compilation_Error;

В самом деле, компилятор не может знать, какой тип фактически будет
использоваться для каждого элемента массива. Но, если мы используем
динамическое распределение памяти используя ссылочные типы, мы сможем
выделять объекты разных типов для отдельных элементов массива :ada:`T_Arr`.
Мы делаем это с помощью надклассовых ссылочных типов, которые имеют
следующий формат:

.. code-block:: ada

    type T_Class is access T'Class;

Мы можем переписать предыдущий пример, используя тип :ada:`T_Class`.
В этом случае динамически выделяемые объекты этого типа будут
диспетчеризироваться
в соответствии с фактическим типом, используемым во время выделения.
Также давайте добавим процедуру :ada:`Init`, которая не будет
переопределена для производного типа :ada:`T_New`. Это адаптированный код:

.. code:: ada run_button project=Courses.Intro_To_Ada.Object_Oriented_Programming.Classwide_Access

    package P is
       type T is tagged record
           E : Integer;
       end record;

       type T_Class is access T'Class;

       procedure Init (A : in out T);

       procedure Show (Dummy : T);

       type T_New is new T with null record;

       procedure Show (Dummy : T_New);

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Init (A : in out T) is
       begin
          Put_Line ("Initializing type T...");
          A.E := 0;
       end Init;

       procedure Show (Dummy : T) is
       begin
          Put_Line ("Using type "
                    & T'External_Tag);
       end Show;

       procedure Show (Dummy : T_New) is
       begin
          Put_Line ("Using type "
                    & T_New'External_Tag);
       end Show;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with P;           use P;

    procedure Main is
      T_Arr  : array (1 .. 2) of T_Class;
    begin
      T_Arr (1) := new T;
      T_Arr (2) := new T_New;

      for I in T_Arr'Range loop
         Put_Line ("Element # "
                   & Integer'Image (I));

         T_Arr (I).Init;
         T_Arr (I).Show;

         Put_Line ("-----------");
      end loop;
    end Main;

В этом примере первый элемент (:ada:`T_Arr (1)`) имеет тип :ada:`T`, а
второй элемент - тип :ada:`T_New`. При запуске примера процедура :ada:`Init`
типа :ada:`T` вызывается для обоих элементов массива :ada:`T_Arr`, в то
время, как вызов процедуры :ada:`Show` выберет нужную
процедуру в соответствии с типом каждого элемента :ada:`T_Arr`.
