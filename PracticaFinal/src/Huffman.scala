
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

  /**
    * Función para pasar de una cadena de texto normal a una lista de caracteres
    *
    * @param cadena
    * @return
    */
  def stringAListaCaracteres(cadena: String) : List[Char] = cadena.toList


  /**
    * Calcula la frecuencia de aparición de cada caracter en el texto a analizar
    *
    * @param texto
    * @return
    */
  def obtenerTuplasOcurrencias(texto: String) : List[(Char,Int)] = {
    val cadena = stringAListaCaracteres(texto)
    cadena.toList.groupBy(c => c).mapValues(_.size).toList
  }


  /**
    * Genera una lista con todos los nodos hojas del árbol de codificación. Debe estar ordenada por pesos de forma ascendente
    *
    * @param caracteres
    * @return
    */
  def generarListHojasOrdenadas(caracteres: List[(Char, Int)]) : List[Nodo] = {
    caracteres.map(caracter => NodoHoja(caracter._1, caracter._2)).sortBy(_.peso)
  }


  /**
    * Comprueba si una lista de nodos contiene un único elemento
    *
    * @param listaNodos
    * @return
    */
  def singleton(listaNodos: List[Nodo]) : Boolean = {
    listaNodos.size == 1
  }


  /**
    * Combina solo los dos primeros nodos de la lista para crear un nodo intermedios
    *
    * @param listaNodos
    * @return
    */
  def combinar(listaNodos: List[Nodo]) : List[Nodo] = {
    val intermedio = generarArbol(listaNodos(0), listaNodos(1))
    val nueva = listaNodos.drop(1).drop(1)
    (intermedio::nueva).sortWith((x,y) => calcularPeso(x) < calcularPeso(y))
  }


  /**
    * Realiza llamadas a métodos anteriores hasta que la lista de nodos contenga un único elemento
    *
    * @param single
    * @param combi
    * @param arboles
    * @return
    */
  def hasta(single: (List[Nodo] => Boolean), combi: (List[Nodo] => List[Nodo]))(arboles: List[Nodo]) : Nodo = {
    if(single(arboles)){
      arboles.head
    }else{
      val nivelUp = combi(arboles)
      hasta(single, combi)(nivelUp)
    }
  }


  /**
    * Función que recibe como argumento la lista de caracteres a analizar y devuelve el árbol generado
    *
    * @param texto
    * @return
    */
  def generarArbolCodificacion(texto: String): Nodo = {
    hasta(singleton, combinar)(generarListHojasOrdenadas(obtenerTuplasOcurrencias(texto)))
  }






}
