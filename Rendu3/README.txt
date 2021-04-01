Bonjour, on a décidé d'inverser les roles pour ce rendu. Donc Mathias a fait le parser/lexer et Emile a fait la partie eval et les tests.

Pour la partie lexer, il n'y a pas grand chose a dire. Pour le parser, il n'y a pas grand chose a dire sur l'ajout de := ! ; ref ou () si ce n'est de faire attention pour les priorités. Nous avons changé le type de prInt et on lui donné un type a lui même pour qu'il ếtre appélé par d'autre fonctions en fouine. Ensuite, pour l'ajout des tuples Mathias créer une nouvelle catégorie les tuples qui sont soit tuples ; une expression ou soit jsute une expression ; une expression.
Ajouter les tuples nous a fait changer notre type pouur le let ... in. Nous pouvons désomé faire des chose du genre let a,b = ... in. On a donc du créer un nouveau type nommé motif qui peut contenir des varibles, des liste ou des tuples.
Pour ajouter les liste, Mathias a fait utiliser la méthode de lire un "[" puis les éléments de la liste et enfin un "]". Il n'y a pas grand chose a dire sur Cons.
Ensuite pour ajouter les matching, nous avons enrichi notre type motif avec les entiers et on voit le matching comme un couple contenant une expression et une liste de motif*expression conenant le motif lu avec la réponse qu'il faut renvoyer en cas. Mathias a fait deux règle pour rentre dans le match permettant de rendre optionnel le premier |.
Ensuite pour les expressions sahcant qu'il n'existe que l'exception E. On a ajouter ajoute uniquement les type raise et try ... with à expr. On voit try with comme un match (dans le cotès parser) sauf que l'on raplace le motif par une E de expression atomique ettant donné que les exection sont tous de la forme E of int. Il n'y a rien a dire sur raise.


RAPE A FORMAGE