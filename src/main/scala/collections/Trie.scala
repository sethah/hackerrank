package collections

trait Trie {
  def prefixes: Long
  def words: Long
  def children: Map[Char, Trie]
  def addWord(word: List[Char]): Trie
  def addWord(s: String): Trie = addWord(s.toList)
  // TODO: getNode should be implemented in the subclasses
  def getNode(word: List[Char]): Trie = {
    word match {
      case Nil => this
      case hd :: tl => children.getOrElse(hd, EmptyTrie).getNode(tl)
    }
  }
  def countPrefixes(word: String): Long = countPrefixes(word.toList)
  def countPrefixes(word: List[Char]): Long = getNode(word).prefixes
  def countWords(word: String): Long = countWords(word.toList)
  def countWords(word: List[Char]): Long = getNode(word).words
}

case class TrieRoot(prefixes: Long, words: Long, children: Map[Char, Trie]) extends Trie {
  def addWord(word: List[Char]): TrieRoot = {
    word match {
      case Nil => this
      case hd :: tl =>
        TrieRoot(0, 0, children + (hd -> children.getOrElse(hd, EmptyTrie).addWord(tl)))
    }
  }
}

case class TrieNode(prefixes: Long, words: Long, children: Map[Char, Trie]) extends Trie {
  def addWord(word: List[Char]): TrieNode = {
    word match {
      case Nil => TrieNode(prefixes + 1, words + 1, Map.empty[Char, Trie])
      case hd :: tl =>
        TrieNode(prefixes + 1, words,
          children + (hd -> children.getOrElse(hd, EmptyTrie).addWord(tl)))
//      case hd :: Nil => {
//        if (children.contains(hd)) {
//          TrieNode(prefixes + 1, words + 1, children)
//        } else {
//          TrieNode(1, words + 1, children + (hd -> EmptyTrie))
//        }
//      }
//      case hd :: tl => {
//        if (children.contains(hd)) {
//          TrieNode(children(hd).prefixes, words, children + (hd -> children(hd).addWord(tl)))
//        } else {
//          TrieNode(1, words, children + (hd -> EmptyTrie.addWord(tl)))
//        }
//      }
    }
  }


}

case object EmptyTrie extends Trie {
  val prefixes: Long = 0
  val words: Long = 0
  val children: Map[Char, Trie] = Map.empty[Char, Trie]
  def addWord(word: List[Char]): Trie = {
    word match {
      case Nil => TrieNode(1, 1, children)
      case hd :: tl =>
        TrieNode(1, 0, Map(hd -> EmptyTrie.addWord(tl)))
    }
  }
}
