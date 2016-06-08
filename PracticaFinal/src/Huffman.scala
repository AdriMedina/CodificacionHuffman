/**
  * Created by Adri Medina on 08/06/2016.
  *
  * Objeto Huffman siguiendo el patrón Singleton para obtener una única instacia de la codificación
  *
  */

object Huffman {

  /**
    * Clase abstracta para representar los nodos del árbol de codificación
    */
  abstract class Nodo

  /**
    * Clase para representar los nodos terminales del árbol de codificación
    *
    * @param caracter
    * @param peso
    */
  case class NodoHoja (caracter: Char, peso: Integer) extends Nodo

  /**
    * Clase para representar los nodos no terminales (intermedios) del árbol de codificación
    *
    * @param izquierda
    * @param derecha
    * @param caracteres
    * @param peso
    */
  case class NodoIntermedio (izquierda: Nodo, derecha: Nodo, caracteres: List[Char], peso: Integer) extends Nodo

  /**
    * Obtiene el peso asociado según el número de apariciones de los caracteres de la lista, considerando los nodos inferiores
    *
    * @param nodo
    * @return
    */
  def calcularPeso(nodo : Nodo) : Int = {
    nodo match {
      case NodoHoja(_, peso) => peso
      case NodoIntermedio(izquierda, derecha, _, _) => calcularPeso(izquierda)+calcularPeso(derecha)
    }
  }

  /**
    * Obtiene la lista de caracteres asociados al nodo, considerando los nodos inferiores
    *
    * @param nodo
    * @return
    */
  def obtenerCaracteres(nodo: Nodo): List[Char] = {
    nodo match {
      case NodoHoja(caracter, _) => List(caracter)
      case NodoIntermedio(izquierda, derecha, _, _) => List.concat(obtenerCaracteres(izquierda), obtenerCaracteres(derecha))
    }
  }

  /**
    * Recibe como argumento los subárboles a la izquierda y derecha y genera un nuevo árbol a partir de ellos
    *
    * @param izquierda
    * @param derecha
    * @return
    */
  def generarArbol(izquierda: Nodo, derecha: Nodo) : Nodo = {
    val peso = calcularPeso(izquierda) + calcularPeso(derecha)
    val caracteres = List.concat(obtenerCaracteres(izquierda), obtenerCaracteres(derecha))
    NodoIntermedio(izquierda, derecha, caracteres, peso)
  }

}
