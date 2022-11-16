package main.scala

trait Tree {
  def getLeftSubtree: Tree
  def getRightSubtree: Tree
  def getNodeData: Int
}

class Node (data: Int, var left: Tree, var right: Tree) extends Tree{
  override def getLeftSubtree: Tree = left
  override def getRightSubtree: Tree = right
  override def getNodeData: Int = data

  def add(el: Int): Unit = {
    if(getNodeData > el) {
      if(getLeftSubtree == null)
        left = new Leaf(el)
      else {
        if(left.isInstanceOf[Leaf])
          left = new Node(left.getNodeData, null, null)
        left.asInstanceOf[Node].add(el)
      }
    } else {
      if (getRightSubtree == null)
        right = new Leaf(el)
      else {
        if (right.isInstanceOf[Leaf])
          right = new Node(right.getNodeData, null, null)
        right.asInstanceOf[Node].add(el)
      }
    }
  }
}

class Leaf (data: Int) extends Tree {
  override def getLeftSubtree: Tree = null
  override def getRightSubtree: Tree = null
  override def getNodeData: Int = data
}

object Application {

  var tree = new Node(8, new Node(3, new Leaf(2), new Leaf(5)), new Node(11, new Leaf(9), null))

  def printTree(tree: Tree): Unit = {
    def printTreeLevel(tree: Tree, lv: Int): Unit = {
      println("-" * lv + tree.getNodeData)
      if (tree.getLeftSubtree != null)
        printTreeLevel(tree.getLeftSubtree, lv + 1)
      if (tree.getRightSubtree != null)
        printTreeLevel(tree.getRightSubtree, lv + 1)
    }
    printTreeLevel(tree, 0)
  }

  def insert(el: Int, tree: Tree): Tree = {
    var newTree: Node = null //new Node(tree.getNodeData, null, null)
    def copyTree(tree: Tree): Node = {
      if(newTree == null)
        newTree = new Node(tree.getNodeData, null, null)
      else
        newTree.add(tree.getNodeData)
      if (tree.getLeftSubtree != null) {
        copyTree(tree.getLeftSubtree)
      }
      if (tree.getRightSubtree != null)
        copyTree(tree.getRightSubtree)
      newTree
    }
    newTree = copyTree(tree)
    newTree.add(el)
    newTree
  }

  def contains(el: Int, tree: Tree): Boolean = {
    var res = false
    def seekInNodes(node: Tree): Boolean = {
      res = res || (node.getNodeData == el)
      if (node.getLeftSubtree != null) {
        seekInNodes(node.getLeftSubtree)
      }
      if (node.getRightSubtree != null) {
        seekInNodes(node.getRightSubtree)
      }
      res
    }
    res = seekInNodes(tree)
    res
  }

  def sum(tree: Tree): Int = {
    var sum = 0
    def sumInNodes(node: Tree): Int = {
      sum += node.getNodeData
      if (node.getLeftSubtree != null) {
        sumInNodes(node.getLeftSubtree)
      }
      if (node.getRightSubtree != null) {
        sumInNodes(node.getRightSubtree)
      }
      sum
    }
    sum = sumInNodes(tree)
    sum
  }

  def main (args: Array[String]): Unit = {
    printTree(tree)
//    printTree(insert(4, tree))
//    printTree(tree)
//    println(contains(9, tree))
//    println(sum(tree))
  }

}
