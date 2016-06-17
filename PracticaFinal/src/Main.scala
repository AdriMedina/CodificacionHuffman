/**
  * Created by Adri Medina on 09/06/2016.
  *
  * Clase para probar la funcionalidad
  *
  */
object Main extends App{

  val texto = "AABCDE"

  val tuplas = Huffman.obtenerTuplasOcurrencias(texto)
  println(tuplas)

  val orden = Huffman.generarListHojasOrdenadas(tuplas)
  println(orden)


  val arbol = Huffman.hasta(Huffman.singleton, Huffman.combinar)(orden)
  println(arbol)


  val texto2 = "Hello world"
  println(Huffman.generarArbolCodificacion(texto2))


  val texto3 = "AAAAAAAABBBCDEFGH"
  val arbol3 = Huffman.generarArbolCodificacion(texto3)
  println(arbol3)

  println(Huffman.convertirArbolTabla(arbol3))

  println(Huffman.decodificar(arbol3, List(0, 1, 1, 1, 1, 0, 1, 1)))





}
