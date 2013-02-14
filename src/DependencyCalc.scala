import java.awt.Dimension
import java.io.File
import scala.Array.canBuildFrom
import scala.collection.Seq
import scala.io.Source
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.XML
import edu.uci.ics.jung.algorithms.layout.CircleLayout
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.SparseMultigraph
import edu.uci.ics.jung.graph.util.EdgeType
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.control.ModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import javax.swing.JFrame
import edu.uci.ics.jung.algorithms.layout.KKLayout

/**
 * Simple command line app to visualise eclipse project dependencies as expressed in .classpath files
 */
object DependencyCalc extends App {

  /**
   * @param f workspace directory
   * @return array of files in this filesystem branch
   */
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles) // would this be better with an immutable collection and an accumulator
  }

  /**
   * @param file full path to .classpath file
   * @return Sequence of pairs (dependent project, target project)
   */
  def readPrjDepsFromFile(file: String): Seq[(String, String)] = {
    val dependencyMap = Map[String, String]()
    val rootElem = XML.loadFile(file)
    val cpEntries = rootElem \\ "classpathentry"
    val srcKindEntries = cpEntries.filter(p =>
      p.attribute("kind").get.text == "src" &&
      !p.attribute("path").get.text.contains("src"))

    val paths = srcKindEntries \\ "@path"
    paths.map(n => (new File(file).getParentFile().getName(), n.toString.replace("/", "")))
  }

  /**
   * @param deps Array of pairs (dependent project, target project) to be displayed as a graph
   */
  def displayGraph(deps: Array[(String, String)]): Unit = {
    val g: Graph[String, String] = new SparseMultigraph[String, String]()

    deps.foreach(f => {
      g.addVertex(f._1)
      g.addVertex(f._2)
      g.addEdge(f._1 + " depends on " + f._2, f._1, f._2, EdgeType.DIRECTED)
    })

    val layout = new KKLayout(g);
    layout.setSize(new Dimension(700, 500));
    
    val vv = new VisualizationViewer[String, String](layout);
    vv.setPreferredSize(new Dimension(750, 550));
    vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller());
    vv.getRenderer().getVertexLabelRenderer().setPosition(Position.AUTO);

    val gm = new DefaultModalGraphMouse();
    gm.setMode(ModalGraphMouse.Mode.PICKING);
    vv.setGraphMouse(gm);

    val frame = new JFrame("Graph Viewer");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.getContentPane().add(vv);
    frame.pack();
    frame.setVisible(true);
  }
  
  // Entry point
  if (args.size == 0) {
    println("Usage: DependencyCalc <WorkspaceFolderFullPath> <project name pattern to exclude> <project name pattern to exclude>...")
    println("E.g. DependencyCalc c:/streams/BEP485/workspace dts web ejb")
    System.exit(-1)
  }

  val workspaceFolder = this.args(0)
  assert(new File(workspaceFolder).isDirectory(), workspaceFolder + " is not a directory")
  
  val otherArgs = this.args.drop(1) // drop is 1-based
  
  val classPathFiles= recursiveListFiles(new File(workspaceFolder)).filter(_.toString.endsWith(".classpath")) 
  assert(!classPathFiles.isEmpty,"No classpath files found")  
  
  val deps = classPathFiles.flatMap(f => readPrjDepsFromFile(f.toString)) // was originally written as: val deps = files.map(f => readFile(f.toString)).flatten  
  assert(!deps.isEmpty, "No inter-project dependencies found")  
  
  val filteredDeps = deps.filterNot(d => otherArgs.exists(s => d._1.toLowerCase().contains(s) || d._2.toLowerCase().contains(s)))  
  assert(!filteredDeps.isEmpty, "No unexcluded inter-project dependencies found")
  
  displayGraph(filteredDeps)
}