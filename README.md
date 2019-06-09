# Multiagent-Action-Language
To do:

### C# GUI ###
- [ ] BŁĄD w Test3_PARSER_ERROR brak parsowania dla *impossible by*
- [ ] Podmiana *not by if* na odpowiednik *impossible by if*
- [ ] Parser na wyrażenie *not by if*
- [ ] Pusta lista agentów
- [ ] Dodać *initially [alpha]* jako *[alpha] after ()* (chodzi o puste wykonanie programu)

### C# reasoning ###
- [ ] Dodanie obsługi *[alpha] observable after ...* i *[alpha] after ...*
- [ ] Testy generowania grafu
- [x] Dodać kwerendę wykonywalności *executable*
- [x] Dodać kwerendę wartości *after*
- [ ] Dodać kwerendę zaangażowania *engaged*
- [x] Dodać testy języka AR (przykłady z wykładu)
- [x] Dodać kwerendy z pustego programu
- [x] Dodać kwerendy dla pustej listy agentów

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


Przykład niedziałającego modelu: <br />
a by [x] causes [f] <br />
a by [y] causes [g] <br />
initially [~f && ~g] <br />
Query: <br />
necessary (f && g) after  (a, [x]), (b, [y]) <br />
Should be: true


Przykład niedziałającego modelu: <br />
a causes [f] <br />
a causes [g] <br />
initially [~f && ~g] <br />
Query: <br />
necessary (f && g) after  (a, [x]), (b, [y]) <br />
Should be: true


Przykład działającego modelu: <br />
a by [x] causes [f] <br />
b by [x] causes [g] <br />
initially [~f && ~g] <br />
Query: <br />
necessary (f && g) after  (a, [x]), (b, [x]) <br />
Should be: true


Przykład niedziałającego modelu: <br />
fire causes [~loaded] <br />
fire causes [~alive] if [loaded] <br />
initially [alive] <br />
[~alive] after (fire, [x]) % tu powinno być bez agenta <br />
Query: <br />
necessary <coś> after [] % to powinno być możliwe do sprawdzenia <br />
Should be: true


Przykład niedziałającego modelu: <br />
cos causes [ble] <br />
fire causes [~loaded] <br />
fire causes [~alive] if [loaded] <br />
initially [alive] <br />
[~alive] after (cos, [x])m (fire, [x]) <br />
Query: <br />
necessary (loaded) after (cos, [x]) <br />
Should be: true
