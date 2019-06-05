# Multiagent-Action-Language
To do:

### C# GUI ###
- [ ] Podmiana *not by if* na odpowiednik *impossible*
- [ ] Parser na wyrażenie *not by if*
- [ ] Parser na kwerendę wykonywalności *executable*
- [ ] Parser na kwerendę wartości *after*
- [ ] Parser na kwerendę zaangażowania *engaged*
- [x] Parser na wyrażenia wartości *after*, *observable after*, *initially*
- [x] Parser na wyrażenia efektu akcji *by causes if*, *impossible by if*
- [x] Parser na wyrażenia uwolnienia fluentu *by releases if*
- [x] Parser na wyrażenia integralności *always*
- [x] Parser na wyrażenie zachowania fluentów *noninertial*
- [x] Parser wyrażeń logicznych do obiektów
- [x] Widoki dla kontrolek
- [x] Parser tworzenia agentów

### C# reasoning ###
- [ ] Testy generowania grafu
- [ ] Dodać kwerendę wykonywalności *executable*
- [ ] Dodać kwerendę wartości *after*
- [ ] Dodać kwerendę zaangażowania *engaged*
- [ ] Dodać testy języka AR (przykłady z wykładu)
- [ ] Dodać kwerendy z pustego programu
- [ ] Dodać kwerendy dla pustej listy agentów

### App tests ###
- [ ] 4 historyjki po 3-4 kwerendy dla grupy 1 **na poniedziałek musi być gotowe**
- [ ] kilka historyjek po 3-4 kwerendy do naszych testów

### Requirements for the deadline ###
- Dokumentacja teoretyczna
- Dokumentacja techniczna
- User Guide
- Testy naszego programu zrobione przez inną grupę **patrz wyżej**
- Testy innego programu zrobione przez nas
- Płytka CD

### Conclusions after deadline meeting ###
- kwerendy mogą też dostać wyrażenie logiczne jako resultat - co wtedy?
- program niewykonalny -> necessary true
- niedeterminizm - releases itp nie działa


Przykład niedziałającego modelu:
a by [x] causes [f]
a by [y] causes [g]
initially [~f && ~g]
Query:
necessary (f && g) after  (a, [x]), (b, [y])
Should be: true


Przykład niedziałającego modelu:
a causes [f]
a causes [g]
initially [~f && ~g]
Query:
necessary (f && g) after  (a, [x]), (b, [y])
Should be: true


Przykład działającego modelu:
a by [x] causes [f]
b by [x] causes [g]
initially [~f && ~g]
Query:
necessary (f && g) after  (a, [x]), (b, [x])
Should be: true


Przykład niedziałającego modelu:
fire causes [~loaded]
fire causes [~alive] if [loaded]
initially [alive]
[~alive] after (fire, [x]) % tu powinno być bez agenta
Query:
necessary <coś> after [] % to powinno być możliwe do sprawdzenia
Should be: true


Przykład niedziałającego modelu:
cos causes [ble]
fire causes [~loaded]
fire causes [~alive] if [loaded]
initially [alive]
[~alive] after (cos, [x])m (fire, [x])
Query:
necessary (loaded) after (cos, [x])
Should be: true
