package collections

object Testing {

//  implicit def stringAsList(s: String): List[Char] = s.toList

  def main(args: Array[String]): Unit = {
    val trie = TrieRoot(0, 0, Map.empty[Char, Trie]).addWord("hello").addWord("world").addWord("worry").addWord("help")
    println(trie)
    println(trie.countPrefixes("hel"))
    val trie2 = trie.addWord("hellish")
    println(trie2)
    println(trie2.countPrefixes("hel"))
    println(trie2.countPrefixes("hellooooo"))
  }
}
