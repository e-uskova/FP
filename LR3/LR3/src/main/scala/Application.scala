package main.scala

trait Tree {
  def getLeftSubtree: Tree
  def getRightSubtree: Tree
  def getNodeData: Int
}

class Node (data: Int) extends Tree{
  var left: Node = null
  var right: Node = null

  override def getLeftSubtree: Node = left
  override def getRightSubtree: Node = right
  override def getNodeData: Int = data
}

class Leaf (data: Int) extends Tree {
  override def getLeftSubtree: Tree = null
  override def getRightSubtree: Tree = null
  override def getNodeData: Int = data
}

class BSTree extends Tree{
  var head: Node = null

  def Add(el: Int): Unit = {
    if(head == null)
      head = new Node(el)
    else
      AddTo(head, el)
  }

  def AddTo(node: Node, el: Int): Unit = {
    if(node.getNodeData > el) {
      if(node.getLeftSubtree == null)
        node.left = new Node(el)
      else AddTo(node.left, el)
    } else {
      if (node.getRightSubtree== null)
        node.right = new Node(el)
      else AddTo(node.right, el)
    }
  }

  override def getLeftSubtree: Node = {
    if (head == null)
      null
    else
      head.getLeftSubtree
  }

  override def getRightSubtree: Node = {
    if (head == null)
      null
    else
      head.getRightSubtree
  }

  override def getNodeData: Int = {
    if (head == null)
      -1
    else
      head.getNodeData
  }

}

object Application {
  def main (args: Array[String]): Unit = {

    def printTree(tree: BSTree): Unit = {
      def printNode(node: Node): Unit = {
        println(node.getNodeData)

//        println("/")
        if (node.getLeftSubtree != null) {
//          print("-")
          printNode(node.getLeftSubtree)
        }
        else println(s"${node.getNodeData} no left")

//        println("\\")
        if (node.getRightSubtree != null) {
//          print("-")
          printNode(node.getRightSubtree)
        }
        else println(s"${node.getNodeData} no right")
      }

      if(tree.getNodeData == -1) println("empty")
      else printNode(tree.head)

    }

    def insertTree(el: Int, tree: BSTree): BSTree = {
      var newTree = new BSTree

      def copyNodes(node: Node): BSTree  = {
        newTree.Add(node.getNodeData)

        if (node.getLeftSubtree != null) {
          copyNodes(node.getLeftSubtree)
        }

        if (node.getRightSubtree != null) {
          copyNodes(node.getRightSubtree)
        }

        newTree
      }

      if(tree.getNodeData == -1) println("empty")
      else newTree = copyNodes(tree.head)
      newTree.Add(el)
      newTree
    }

    val testTree = new BSTree

    testTree.Add(8)
    testTree.Add(3)
    testTree.Add(5)
    testTree.Add(2)
    testTree.Add(11)
    testTree.Add(9)

    printTree(testTree)
    printTree(insertTree(4, testTree))
  }
}
