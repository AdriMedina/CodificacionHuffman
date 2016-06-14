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
  println(Huffman.generarArbolCodificacion(texto3))
}
