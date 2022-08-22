Изоляция
========

.. include:: ../../global.txt

Одним из основных положений модульного программирования, а также
объектно-ориентированного программирования, является  `инкапсуляция
<https://ru.wikipedia.org/wiki/Инкапсуляция_(программирование)>`_.

Инкапсуляция, вкратце, является концепцией, следуя которой разработчик
программного обеспечения разделяет общедоступный интерфейс подсистемы
и ее внутреннюю реализацию.

Это касается не только библиотек программного обеспечения, но и
всего, где используются абстракции.

Ада несколько отличается от большинства объектно-ориентированных языков,
тем что границы инкапсуляции в основном проходят по границам пакетов.

Простейшая инкапсуляция
-----------------------

.. code:: ada compile_button project=Courses.Intro_To_Ada.Privacy.Encapsulate
    :class: ada-expect-compile-error

    package Encapsulate is
       procedure Hello;

    private

       procedure Hello2;
       --  Not visible from external units
    end Encapsulate;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Encapsulate is

       procedure Hello is
       begin
          Put_Line ("Hello");
       end Hello;

       procedure Hello2 is
       begin
          Put_Line ("Hello #2");
       end Hello2;

    end Encapsulate;

    with Encapsulate;

    procedure Main is
    begin
       Encapsulate.Hello;
       Encapsulate.Hello2;
       --  Invalid: Hello2 is not visible
    end Main;

Абстрактные типы данных
-----------------------

С таким высокоуровневым механизмом инкапсуляции может быть неочевидно, как
скрыть детали реализации одного типа. Вот как это можно сделать в Аде:

.. code:: ada no_button project=Courses.Intro_To_Ada.Privacy.Stacks
    :class: ada-syntax-only

    package Stacks is
       type Stack is private;
       --  Declare a private type: You cannot depend
       --  on its implementation. You can only assign
       --  and test for equality.

       procedure Push (S   : in out Stack;
                       Val :        Integer);
       procedure Pop (S   : in out Stack;
                      Val :    out Integer);
    private

       subtype Stack_Index is Natural range 1 .. 10;
       type Content_Type is array (Stack_Index)
         of Natural;

       type Stack is record
          Top     : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

    package body Stacks is

       procedure Push (S   : in out Stack;
                       Val :        Integer) is
       begin
          --  Missing implementation!
          null;
       end Push;

       procedure Pop (S   : in out Stack;
                      Val :    out Integer) is
       begin
          --  Dummy implementation!
          Val := 0;
       end Pop;

    end Stacks;

В приведенном выше примере мы определяем тип для стека в публичной части
(известной как *видимый раздел* спецификации пакета в Аде), но детали
реализация этого типа скрыты.

Затем в личном разделе мы определяем реализацию этого типа. Мы также
можем объявить там другие *вспомогательные* типы, которые будут использованы
для описания основного публичного типа. Создание вспомогательных типов - это
полезная и распространенная практика в Аде.

Несколько слов о терминологии:

-  То, как выглядит тип стека :ada:`Stack` в видимом разделе, называется
   частичным представлением типа. Это то, к чему имеют доступ клиенты.

-  То, как выглядит тип стека :ada:`Stack` из личного раздела или тела пакета,
   называется полным представлением типа. Это то, к чему имеют доступ
   разработчики.

С точки зрения клиента (указывающего пакет в :ada:`with`) важен только
видимый раздел, и личного вообще может не существовать. Это позволяет
очень легко просмотреть ту часть пакета, которая важна для нас.

.. code-block:: ada

    --  No need to read the private part to use the package
    package Stacks is
       type Stack is private;

       procedure Push (S   : in out Stack;
                       Val :        Integer);
       procedure Pop (S   : in out Stack;
                      Val :    out Integer);
    private
       ...
    end Stacks;

А вот как будет использоваться пакет :ada:`Stacks`:

.. code-block:: ada

    --  Example of use
    with Stacks; use Stacks;

    procedure Test_Stack is
       S : Stack;
       Res : Integer;
    begin
       Push (S, 5);
       Push (S, 7);
       Pop (S, Res);
    end Test_Stack;

Лимитируемые типы
-----------------

В Аде конструкция *лимитируемого типа* позволяет вам объявить тип, для
которого операции присваивания и сравнения не предоставляются
автоматически.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Privacy.Limited_Stacks
    :class: ada-expect-compile-error

    package Stacks is
       type Stack is limited private;
       --  Limited type. Cannot assign nor compare.

       procedure Push (S   : in out Stack;
                       Val :        Integer);
       procedure Pop (S   : in out Stack;
                      Val :    out Integer);
    private
       subtype Stack_Index is Natural range 1 .. 10;
       type Content_Type is
         array (Stack_Index) of Natural;

       type Stack is limited record
          Top     : Stack_Index;
          Content : Content_Type;
       end record;
    end Stacks;

    package body Stacks is

       procedure Push (S   : in out Stack;
                       Val :        Integer) is
       begin
          --  Missing implementation!
          null;
       end Push;

       procedure Pop (S   : in out Stack;
                      Val :    out Integer) is
       begin
          --  Dummy implementation!
          Val := 0;
       end Pop;

    end Stacks;

    with Stacks; use Stacks;

    procedure Main is
       S, S2 : Stack;
    begin
       S := S2;
       --  Illegal: S is limited.
    end Main;

Это нужно, например, для тех типов данных, для которых
встроенная операция присваивания работает неправильно (например,
когда требуется многоуровневевое копирование).

Ада позволяет вам определить операторы сравнения :ada:`=` и :ada:`/=` для
лимитируемых типов (или переопределить встроенные объявления для
нелимитируемых).

Ада также позволяет вам предоставить собственную реализацию присваивания
используя `контролируемые типы
<https://www.adaic.org/resources/add_content/standards/12rm/html/RM-7-6.html>`_.
Однако в некоторых случаях операция присваивания просто не имеет смысла;
примером может служить :ada:`File_Type` из пакета :ada:`Ada.Text_IO`, который
объявлен как лимитируемый тип, и поэтому все попытки присвоить один файл
другому будут отклонены как незаконные.

Дочерние пакеты и изоляция
--------------------------

Ранее мы видели (в :ref:`разделе дочерние пакеты <Intro_Ada_Ru_ChildPackages>`),
что пакеты могут иметь дочерние пакеты. Изоляция играет важную роль
в дочерних пакетах. В этом разделе обсуждаются некоторые правила
касающиеся изоляции, действующие для дочерних пакетов.

Хотя личный раздел :ada:`P` предназначен для инкапсуляции информации,
некоторые части дочернего пакета :ada:`P.C` могут иметь доступ к этому личному
разделу :ada:`P`. В таких случаях информация из личного раздела :ada:`P` может
затем использоваться так, как если бы она была объявлена в видимом разделе
спецификации пакета. Говоря более конкретно, тело :ada:`P.C` и личный раздел
пакета :ada:`P.C` имеют доступ к личному разделу :ada:`P`. Однако
видимый раздел спецификации :ada:`P.C` имеет доступ только к видимому разделу
спецификации :ada:`P`. В следующей таблице приводится сводная информация об
этом:

+-------------------------------+-------------------------------+
| Часть дочернего пакета        | Доступ к личному разделу      |
|                               | родительской спецификации     |
+===============================+===============================+
| Спецификация: видимый раздел  | Нет                           |
+-------------------------------+-------------------------------+
| Спецификация: личный раздел   | Да                            |
+-------------------------------+-------------------------------+
| Тело                          | Да                            |
+-------------------------------+-------------------------------+

В оставшейся части этого раздела показаны примеры того, как этот
доступ к личной информации на самом деле работает для дочерних
пакетов.

Давайте сначала рассмотрим пример, в котором тело дочернего пакета :ada:`P.C`
имеет доступ к личному разделу спецификации его родителя :ada:`P`. В
предыдущем примере исходного кода мы видели, что процедуру :ada:`Hello2`,
объявленную в личном разделе пакета :ada:`Encapsulate` нельзя использовать в
процедуре :ada:`Main`, поскольку ее там не видно. Однако это ограничение
не распространяется на некоторые части дочерних пакетов.
Фактически, тело дочернего пакета :ada:`Encapsulate.Child` имеет доступ
к процедуре :ada:`Hello2` и она может быть вызыванна оттуда, как вы можете
видеть в реализации процедуры :ada:`Hello3` пакета :ada:`Child`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Privacy.Encapsulate_Child

    package Encapsulate is
       procedure Hello;

    private

       procedure Hello2;
       --  Not visible from external units
       --  But visible in child packages
    end Encapsulate;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Encapsulate is

       procedure Hello is
       begin
          Put_Line ("Hello");
       end Hello;

       procedure Hello2 is
       begin
          Put_Line ("Hello #2");
       end Hello2;

    end Encapsulate;

    package Encapsulate.Child is

       procedure Hello3;

    end Encapsulate.Child;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Encapsulate.Child is

       procedure Hello3 is
       begin
          --  Using private procedure Hello2
          --  from the parent package
          Hello2;
          Put_Line ("Hello #3");
       end Hello3;

    end Encapsulate.Child;

    with Encapsulate.Child;

    procedure Main is
    begin
       Encapsulate.Child.Hello3;
    end Main;

Тот же механизм применяется к типам, объявленным в личном разделе
родительского пакета. Например, тело дочернего пакета может получить
доступ к компонентам записи, объявленной в личном разделе его
родительского пакета. Рассмотрим пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Privacy.Private_Type_Child

    package My_Types is

       type Priv_Rec is private;

    private

       type Priv_Rec is record
         Number : Integer := 42;
       end record;

    end My_Types;

    package My_Types.Ops is

       procedure Display (E : Priv_Rec);

    end My_Types.Ops;

    with Ada.Text_IO; use Ada.Text_IO;

    package body My_Types.Ops is

       procedure Display (E : Priv_Rec) is
       begin
          Put_Line ("Priv_Rec.Number: "
                    & Integer'Image (E.Number));
       end Display;

    end My_Types.Ops;

    with Ada.Text_IO;  use Ada.Text_IO;

    with My_Types;     use My_Types;
    with My_Types.Ops; use My_Types.Ops;

    procedure Main is
       E : Priv_Rec;
    begin
       Put_Line ("Presenting information:");

       --  The following code would trigger a
       --  compilation error here:
       --
       --  Put_Line ("Priv_Rec.Number: "
       --            & Integer'Image (E.Number));

       Display (E);
    end Main;

В этом примере у нас нет доступа к компоненте :ada:`Number` типа записи
:ada:`Priv_Rec` в процедуре :ada:`Main`. Вы можете увидеть это в вызове
:ada:`Put_Line`, который был закомментирован в реализации :ada:`Main`.
Попытка получить доступ к компоненте :ada:`Number`
вызовет ошибку компиляции. Но у нас есть доступ к этой компоненте в
теле пакета :ada:`My_Types.Ops`, поскольку это дочерний пакет пакета
:ada:`My_Types`. Следовательно, тело :ada:`Ops` имеет доступ к объявлению
типа :ada:`Priv_Rec`, которое находится в личном разделе его родительского
пакета :ada:`My_Types`. По этой причине тот же вызов :ada:`Put_Line`,
который вызовет ошибку компиляции в процедуре :ada:`Main`, отлично работает в
процедуре :ada:`Display` пакета :ada:`My_Types.Ops`.

Такой рода правила изоляции для дочерних пакетов позволяют
расширять функциональность родительского пакета и в то же время
обеспечивают инкапсуляцию.

Как мы упоминали ранее, в дополнение к телу пакета личный раздел
спецификации дочернего пакета :ada:`P.C` также имеет доступ к личному разделу
спецификации его родителя :ada:`P`. Давайте посмотрим на пример, в
котором мы объявляем объект личного типа :ada:`Priv_Rec` в личном разделе
дочернего пакета :ada:`My_Types.Child` *и* напрямую инициализируем
компоненту :ada:`Number` записи :ada:`Priv_Rec`:

.. code-block:: ada

    package My_Types.Child is

    private

       E : Priv_Rec := (Number => 99);

    end My_Types.Ops;

Естественно, мы не смогли бы инициализировать этот компонент, если
бы переместили это объявление в общедоступный (видимый) раздел того же
дочернего пакета:

.. code-block:: ada

    package My_Types.Child is

       E : Priv_Rec := (Number => 99);

    end My_Types.Ops;

Объявление выше вызывает ошибку компиляции, поскольку тип :ada:`Priv_Rec`
является личным. Поскольку видимый раздел :ada:`My_Types.Child`
также виден за пределами дочернего пакета, Ада запрещает доступ
к личной информации в этом разделе спецификации.


