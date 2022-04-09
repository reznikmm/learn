Объектно-ориентированное программирование
=========================================

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Объектно-ориентированное программирование (ООП) - это большая и плохо
определенная концепция в языках программирования, которая имеет
тенденцию включать в себя множество разных значений, потому что разные
языки часто реализуют свое собственное видение этого, со сходствами и
отличиями от реализаций на других языках.

Однако одна модель в основном «выиграла» битву за то, что такое
объектно-ориентированный подход, хотя бы благодаря чистой
популярности. Это модель, используемая в языке программирования Java,
которая очень похожа на модель, используемую в C ++. Вот некоторые
определяющие характеристики:

-  Вывод и расширение типов: Большинство объектно-ориентированных языков
   позволяют пользователю добавлять поля в производные типы.

-  Подтипирование: Объекты типа, производного от базового типа, в
   некоторых случаях могут быть заменены объектами базового типа.

-  Полиморфизм среды выполнения: Вызов подпрограммы, обычно называемой
   *методом*, прикрепленной к типу объекта, может выполняться во время
   выполнения в зависимости от точного типа объекта.

-  Инкапсуляция: Объекты могут скрывать некоторые свои данные.

-  Расширяемость: пользователи "извне" вашего пакета или даже всей вашей
   библиотеки могут извлекать данные из ваших типов объектов и определять
   свое собственное поведение.

Ada появилась еще до того, как объектно-ориентированное
программирование стало таким же популярным, как и сегодня. Некоторые
механизмы и концепции из приведенного выше списка были в самой ранней
версии Ada еще до того, как было добавлено то, что мы бы назвали ООП:

-  Как мы видели, инкапсуляция реализована не на уровне типа в Ada, а на
   уровне пакета.

-  Подтипирование может быть реализовано с использованием, ну, подтипов,
   которые имеют полную и разрешительную статическую модель замещаемости.
   Замена завершится неудачно во время выполнения, если динамические
   ограничения подтипа не будут выполнены.

-  Полиморфизм времени выполнения может быть реализован с использованием
   записей вариантов.

Однако в этом списке не указаны расширения типов, если вы не
учитываете записи вариантов и расширяемость.

Редакция Ada 1995 года добавила функцию, заполняющую пробелы, которая
позволила людям проще программировать, следуя объектно-ориентированной
парадигме. Эта функция называется *помеченными* (маркированными или
тегированными) *типами*.

.. note::
    Примечание: В Ada можно программировать, даже не создавая помеченные
    типы. Если это ваш предпочтительный стиль программирования или у вас
    нет специального применения для помеченных типов, не стесняйтесь не
    использовать их, как в случае со многими функциями Ada.

    Тем не менее, они могут быть лучшим способом выразить решения
    определенных проблем, и они могут быть лучшим способом решить вашу
    проблему. Если это так, читайте дальше!

Производные типы
----------------

Прежде чем представить помеченные типы, мы должны обсудить тему,
которую мы затронули, но на самом деле не затрагивали до сих пор:

Вы можете создать один или несколько новых типов из каждого типа в
Ada. Вывод типов встроен в язык.

.. code-block:: ada

    package Newtypes is
       type Point is record
           X, Y : Integer;
       end record;

       type New_Point is new Point;
    end Newtypes;

Наследование типов полезно для обеспечения строгой типизации,
поскольку система типов рассматривает эти два типа как несовместимые.

Но преимущества этим не ограничиваются: вы можете унаследовать вещи от
того типа, от которого произошли. Вы не только наследуете
представление данных, но также можете наследовать поведение.

Когда вы наследуете тип, вы также наследуете так называемые
примитивные операции. *Примитивная операция* (или просто *примитив*) -
это подпрограмма, прикрепленная к типу. Ada определяет примитивы как
подпрограммы, определенные в той же области, что и тип.

.. attention::
    Обратите внимание: подпрограмма станет примитивом этого типа только в
    том случае, если:

    1. Подпрограмма объявляется в той же области, что и тип и
    2. Тип и подпрограмма объявляются в пакете.

.. code-block:: ada

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
типами записей (вы можете использовать его для дискретных типов, как в
примере выше), но он лишь внешне похож на объектно-ориентированное
наследование:

-  Записи не могут быть расширены только с помощью этого механизма. Вы
   также не можете указать новое представление для нового типа: оно
   **всегда** будет иметь то же представление, что и базовый тип.

-  Нет возможности для динамической диспетчеризации или полиморфизма.
   Объекты имеют фиксированный, статический тип.

Есть и другие различия, но перечислять их все здесь бесполезно. Просто
помните, что это своего рода наследование, которое вы можете
использовать, если хотите только статически наследовать поведение без
дублирования кода или использования композиции, но которое вы не
можете использовать, если вам нужны какие-либо динамические функции,
которые обычно связаны с ООП.

Типы с тегами
-------------

Версия языка Ada 1995 года представила тегированные типы, чтобы
удовлетворить потребность в унифицированном решении, которое позволяет
программировать в объектно-ориентированном стиле, аналогичном тому,
который описан в начале этой главы.

Типы с тегами очень похожи на обычные записи, за исключением того, что
добавлены некоторые функции:

-  Типы имеют *тег*, хранящийся внутри каждого объекта, который определяет
   `тип среды выполнения <https://ru.wikipedia.org/wiki/Динамическая_идентификация_типа_данных>`_
   этого объекта.

-  Примитивы можно отправлять. Примитив для помеченного типа - это то,
   что вы бы назвали *методом* в Java или C ++. Если вы производите
   базовый тип и переопределяете его примитив, вы часто можете вызвать
   его для объекта, в результате чего то, какой примитив вызывается,
   зависит от точного типа среды выполнения объекта.

-  Введены правила выделения подтипов, позволяющие тегированному типу,
   производному от базового типа, быть статически совместимым с базовым
   типом.

Давайте посмотрим на наши первые объявления тегированного типа:

.. code-block:: ada

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

Типы расширяемые классами
-------------------------

Чтобы оставаться согласованным с остальной частью языка, необходимо
было ввести новую нотацию, чтобы сказать: "Этот объект относится к
этому типу или любому потомку, производному от тегированного типа".

В Ada мы называем это типы *расширяемые классами*. Он используется в
ООП, как только вам понадобится полиморфизм. Например, вы не можете
выполнить следующие действия:

.. code-block:: ada
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

Это связано с тем, что объект типа :ada:`T` точно соответствует типу :ada:`T`,
независимо от того, :ada:`T` помечен как теговый или нет. То, что вы хотите
сказать как программист, это «Я хочу, чтобы O3 могли держать объект
типа :ada:`My_Class` или любого типа, нисходящего от :ada:`My_Class`.»
Вот как вы это делаете:

.. code-block:: ada

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
    Обратите внимание: Поскольку объект типа класса может быть размером
    любого потомка его базового типа, он имеет неизвестный размер. Таким
    образом, это неопределенный тип с ожидаемыми ограничениями:

    - Он не может быть сохранен как поле/компонент записи

    - Объект типа класса должен быть инициализирован немедленно (вы не
      можете указать ограничения такого типа каким-либо иным способом, кроме
      как путем его инициализации).

Операции диспетчеризации
------------------------

Мы увидели, что можно переопределить операции в типах, производных от
другого типа с тегами. Конечной целью ООП является выполнение
диспетчерского вызова: вызова примитива (метода), который зависит от
точного типа объекта.

Но если задуматься, переменная типа :ada:`My_Class` всегда содержит объект
именно такого типа. Если требуется переменная, которая может содержать
:ada:`My_Class` или любой производный тип, она должна иметь тип
:ada:`My_Class'Class`.

Другими словами, чтобы сделать вызов диспетчеризации, вы должны
сначала иметь объект, который может быть либо конкретным типом, либо
любым типом, производным от этого конкретного типа, а именно объект
классового типа.

.. code-block:: ada

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
    :ada:`My_Class`. Это называется
    *преобразованием представления* на языке Ada и полезно, например, если
    вы хотите вызвать родительский метод.

    В этом случае объект действительно преобразуется в объект :ada:`My_Class`,
    что означает изменение его тега. Поскольку теговые объекты всегда
    передаются по ссылке, вы можете использовать этот вид преобразования
    для изменения состояния объекта: изменения в преобразованном объекте
    повлияют на оригинал.

    .. code-block:: ada

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

Вы также можете вызывать примитивы тегированных типов с помощью
нотации, более знакомой объектно-ориентированным программистам.
Учитывая приведенный выше примитив Foo, вы также можете написать
указанную выше программу следующим образом:

.. code-block:: ada

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

Если диспетчерский параметр примитива является первым параметром, как
в наших примерах, вы можете вызвать примитив, используя точечную
нотацию. Любой оставшийся параметр передается нормально:

.. code-block:: ada

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

Частные и ограниченные типы с тегами
------------------------------------

Ранее мы видели (в главе :doc:`./privacy`), что типы могут быть
объявлены ограниченными или частными. Эти методы инкапсуляции также
могут применяться к тегированным типам, как мы увидим в этом разделе.

Это пример закрытого типа с тегами:

.. code-block:: ada

    package P is
       type T is tagged private;
    private
       type T is tagged record
           E : Integer;
       end record;
    end P;

Это пример ограниченного типа с тегами:

.. code-block:: ada

    package P is
       type T is tagged limited record
           E : Integer;
       end record;
    end P;

Естественно, вы можете комбинировать как *ограниченные*, так и *частные*
типы и объявить ограниченный частный тип с тегами:

.. code-block:: ada

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

Обратите внимание, что код в процедуре :ada:`Main` представляет два назначения,
которые вызывают ошибки компиляции, потому что тип :ada:`T` является
ограниченным частным. Фактически, вы не можете:

-  присваивать :ada:`T1.E` напрямую, потому что тип :ada:`T` является частным;

-  присваивать :ada:`T1` в :ada:`T2`, потому что тип :ada:`T` ограничен.

В этом случае нет различия между тегами и без тегов: эти ошибки
компиляции также могут возникать для типов и без тегов.

Типы доступа в классах
----------------------

В этом разделе мы обсудим полезный шаблон для
объектно-ориентированного программирования в Ada: общеклассовый тип
доступа. Начнем с примера, в котором мы объявляем тегированный тип :ada:`T` и
производный тип :ada:`T_New`:

.. code-block:: ada

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

Обратите внимание, что мы используем null записи для типов :ada:`T` и
:ada:`T_New`. Хотя эти типы на самом деле не имеют каких-либо компонентов,
мы по-прежнему можем использовать их для демонстрации диспетчеризации.
Также обратите внимание, что в приведенном выше примере используется
атрибут :ada:`'External_Tag` в реализации процедуры :ada:`Show` для
получения строки для соответствующего типа тегов.

Как мы видели ранее, мы должны использовать классический тип для
создания объектов, которые могут выполнять диспетчерские вызовы.
Другими словами, будут отправляться объекты типа :ada:`T'Class`. Например:

.. code-block:: ada

    with P; use P;

    procedure Dispatching_Example is
      T2         :          T_New;
      T_Dispatch : constant T'Class := T2;
    begin
      T_Dispatch.Show;
    end Dispatching_Example;

Более полезным приложением является объявление массива объектов,
которые могут выполнять диспетчеризацию. Например, мы хотели бы
объявить массив :ada:`T_Arr`, выполнить цикл по этому массиву и выполнять
диспетчеризацию в соответствии с фактическим типом каждого отдельного
элемента:

.. code-block:: ada

    for I in T_Arr'Range loop
       T_Arr (I).Show;
       --  Call Show procedure according
       --  to actual type of T_Arr (I)
    end loop;

Однако напрямую объявить массив типа :ada:`T'Class` невозможно:

.. code-block:: ada
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

Фактически, компилятор не может знать, какой тип фактически будет
использоваться для каждого элемента массива. Если мы используем
динамическое распределение через типы доступа, мы можем выделять
объекты разных типов для отдельных элементов массива :ada:`T_Arr`.
Мы делаем это с помощью общеклассовых типов доступа, которые имеют
следующий формат:

.. code-block:: ada

    type T_Class is access T'Class;

Мы можем переписать предыдущий пример, используя тип :ada:`T_Class`.
В этом случае динамически выделяемые объекты этого типа будут отправляться
в соответствии с фактическим типом, используемым во время выделения.
Также давайте представим процедуру :ada:`Init`, которая не будет
переопределена для производного типа :ada:`T_New`. Это адаптированный код:

.. code-block:: ada

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
второй элемент ‑ тип :ada:`T_New`. При запуске примера процедура :ada:`Init`
типа :ada:`T` вызывается для обоих элементов массива :ada:`T_Arr`, в то
время как вызов процедуры :ada:`Show` выбирает соответствующую
процедуру в соответствии с типом каждого элемента :ada:`T_Arr`.
