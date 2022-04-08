Конфиденциальность
==================

.. include:: ../../global.txt

Одним из основных положений модульного программирования, а также
объектно-ориентированного программирования, является  `инкапсуляция
<https://ru.wikipedia.org/wiki/Инкапсуляция_(программирование)>`_.

Инкапсуляция, вкратце, является концепцией, что исполнитель части
программного обеспечения будет различать общедоступный интерфейс кода
и его частную реализацию.

Это применимо не только к библиотекам программного обеспечения, но и
везде, где используется абстракция.

В Ada гранулярность инкапсуляции немного отличается от большинства
объектно-ориентированных языков, потому что конфиденциальность обычно
задается на уровне пакета.

Базовая инкапсуляция
--------------------

.. code-block:: ada
    :class: ada-expect-compile-error

    package Encapsulate is
       procedure Hello;

    private

       procedure Hello2;
       --  Not visible from external units
    end Encapsulate;

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

С такой высокоуровневой детализацией может показаться неочевидным, как
скрыть детали реализации типа. Вот как это можно сделать в Ada:

.. code-block:: ada
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

В приведенном выше примере мы определяем тип стека в публичной части
(известной как *видимая часть* спецификации пакета в Ada), но точное
представление этого типа является частным.

Затем в частной части мы определяем представление этого типа. Мы также
можем объявить другие типы, которые будут использоваться в качестве
*помощников* для нашего основного публичного типа. Это полезно,
поскольку объявление типов помощника распространено в Ada.

Несколько слов о терминологии:

-  Тип стека :ada:`Stack`, рассматриваемый из общедоступной части, называется
   частичным представлением типа. Это то, к чему клиенты имеют доступ.

-  Тип стека :ada:`Stack`, рассматриваемый из частной части или тела пакета,
   называется полным представлением типа. Это то, к чему имеют доступ
   разработчики.

С точки зрения клиента (*коммутатора*) важна только публичная (видимая)
часть, а закрытая часть также может не существовать. Это позволяет
очень легко линейно прочитать ту часть пакета, которая важна для вас.

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

Вот как будет использоваться пакет :ada:`Stacks`:

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

Ограниченные типы
-----------------

Возможность *ограниченного типа* в Ada позволяет вам объявить тип, для
которого операции присваивания и сравнения не предоставляются
автоматически.

.. code-block:: ada
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

Это полезно, потому что, например, для некоторых типов данных
встроенная операция присваивания может быть неправильной (например,
когда требуется многоуровневая копия).

Ada позволяет вам перегрузить операторы сравнения :ada:`=` и :ada:`/=` для ограниченных
типов (и переопределить встроенные объявления для неограниченных
типов).

Ada также позволяет реализовать специальную семантику для присваивания
через `контролируемые типы 
<https://www.adaic.org/resources/add_content/standards/12rm/html/RM-7-6.html>`_.
Однако в некоторых случаях назначение
просто неуместно; одним из примеров является :ada:`File_Type` из пакета
:ada:`Ada.Text_IO`, который объявлен как ограниченный тип, и поэтому попытки
назначить один файл другому будут обнаружены как незаконные.

Дочерние пакеты и конфиденциальность
------------------------------------

Ранее мы видели (в :ref:`разделе дочерние пакеты <ChildPackages>`), что
пакеты могут иметь дочерние пакеты. Конфиденциальность играет важную роль
в дочерних пакетах. В этом разделе обсуждаются некоторые правила
конфиденциальности, применимые к дочерним пакетам.

Хотя закрытая часть пакета :ada:`P` предназначена для инкапсуляции информации,
некоторые части дочернего пакета :ada:`P.C` могут иметь доступ к этой закрытой
части :ada:`P`. В этих случаях информация из частной части :ada:`P` может затем
использоваться так, как если бы она была объявлена в общедоступной
части ее спецификации. Чтобы быть более конкретным, тело :ada:`P.C` и частная
часть спецификации :ada:`P.C` имеют доступ к частной части :ada:`P`. Однако
публичная часть спецификации :ada:`P.C` имеет доступ только к публичной части
спецификации :ada:`P`. В следующей таблице приводится краткая информация об
этом:

+-------------------------------+-------------------------------+
| Часть дочернего пакета        | Доступ к частной части        |
|                               | родительской спецификации     |
+===============================+===============================+
| Спецификация: публичная часть | Да                            |
+-------------------------------+-------------------------------+
| Спецификация: частная часть   | Нет                           |
+-------------------------------+-------------------------------+
| Тело                          | Нет                           |
+-------------------------------+-------------------------------+

В оставшейся части этого раздела показаны примеры того, как этот
доступ к частной информации на самом деле работает для дочерних
пакетов.

Давайте сначала рассмотрим пример, в котором тело :ada:`P.C` с дочерним пакетом
имеет доступ к частной части спецификации его родительского :ada:`P`. В
предыдущем примере исходного кода мы видели, что процедура :ada:`Hello2`,
объявленная в частной части пакета :ada:`Encapsulate` нельзя использовать в
процедуре :ada:`Main`, так как он там не отображается. Однако это ограничение
не распространяется на части дочерних пакетов пакета :ada:`Encapsulate`.
Фактически, тело его дочернего пакета :ada:`Encapsulate.Child` имеет доступ
к процедуре :ada:`Hello2` и может вызывать ее там, как вы можете видеть 
в реализации процедуры :ada:`Hello3` пакета :ada:`Child`:

.. code-block:: ada

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

Тот же механизм применяется к типам, объявленным в частной части
родительского пакета. Например, тело дочернего пакета может получить
доступ к компонентам записи, объявленной в частной части его
родительского пакета. Рассмотрим пример:

.. code-block:: ada

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

В этом примере у нас нет доступа к компоненту :ada:`Number` типа записи
:ada:`Priv_Rec` в процедуре :ada:`Main`. Вы можете увидеть это в вызове
:ada:`Put_Line`, который был закомментирован в реализации :ada:`Main`.
Попытка получить доступ к компоненту :ada:`Number`
вызовет ошибку компиляции. Но у нас есть доступ к этому компоненту в
теле пакета :ada:`My_Types.Ops`, поскольку это дочерний пакет пакета
:ada:`My_Types`. Следовательно, тело :ada:`Ops` имеет доступ к объявлению
типа :ada:`Priv_Rec`, которое находится в частной части его родительского
пакета :ada:`My_Types`. По этой причине тот же вызов :ada:`Put_Line`,
который вызовет ошибку компиляции в процедуре :ada:`Main`, отлично работает в
процедуре :ada:`Display` пакета :ada:`My_Types.Ops`.

Такой вид правил конфиденциальности для дочерних пакетов позволяет
расширить функциональность родительского пакета и в то же время
сохранить его инкапсуляцию.

Как мы упоминали ранее, в дополнение к телу пакета частная часть
спецификации дочернего :ada:`P.C` пакетом также имеет доступ к частной части
спецификации его родительского :ada:`P`. Давайте посмотрим на пример, в
котором мы объявляем объект частного введите :ada:`Priv_Rec` в частной части
дочернего пакета :ada:`My_Types.Child` и *and* напрямую инициализируйте
компонент :ada:`Number` записи :ada:`Priv_Rec`:

.. code-block:: ada

    package My_Types.Child is

    private

       E : Priv_Rec := (Number => 99);

    end My_Types.Ops;

Как и ожидалось, мы не смогли бы инициализировать этот компонент, если
бы переместили это объявление в общедоступную (видимую) часть того же
дочернего пакета:

.. code-block:: ada

    package My_Types.Child is

       E : Priv_Rec := (Number => 99);

    end My_Types.Ops;

Объявление выше вызывает ошибку компиляции, поскольку тип :ada:`Priv_Rec`
является закрытым. Поскольку общедоступная часть :ada:`My_Types.Child`
также видна за пределами дочернего пакета, Ada не может разрешить доступ
к частной информации в этой части спецификации.


