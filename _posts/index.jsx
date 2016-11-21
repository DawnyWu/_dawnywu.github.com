class Model {
  
  constructor() {
    this.historyIndex = 0;
    this.history = Immutable.List.of(Immutable.List());
  }

  undo() {
    if (this.historyIndex > 0) 
      this.historyIndex--;
    
     this.update();
  }
  
  redo() {
    if (this.historyIndex < this.history.size) 
      this.historyIndex++;
    
    this.update();
  }
  
  addPoint(point) {
    this.performOperation(data => {
      return data.push(Immutable.Map({
        x: point.x,
        y: point.y,
        id: +new Date()
      }))
    });
  }
  
  performOperation(fn) {
    this.history = this.history.slice(0, this.historyIndex + 1);
    let newVersion = fn( this.history.get(this.historyIndex));
    this.history = this.history.push(newVersion);
    this.historyIndex++;
    
    this.update();
  }
  
  update() {
    renderComponent({points: this.history, historyIndex: this.historyIndex});
  }
}

var model = new Model();

class DrawingCanvas extends React.Component {
  
  handleUndo() { model.undo(); }
  handleRedo() { model.redo(); }
  
  handleArtboardClick(event) {
    let point = {
      x: event.clientX - event.target.getBoundingClientRect().left,
      y: event.clientY - event.target.getBoundingClientRect().top
    };
    model.addPoint(point);
  }
  
  render() {
    
    var redoDisabled = true;
    var undoDisabled = true;
    var circles = [];
    
    if (this.props.points && this.props.historyIndex) {
      this.props.points.get(this.props.historyIndex).forEach(point => {
          circles.push(<circle key={point.get('id')} r="5" cx={point.get('x')} cy={point.get('y')} fill="#FFFFFF" />);
        });

      undoDisabled = this.props.historyIndex === 0;
      redoDisabled = this.props.historyIndex === this.props.points.size - 1;
    }

    return (
      <div className="drawing-canvas container">
      
        <svg ref="artboard" 
             className="artboard container" 
             onClick={this.handleArtboardClick.bind(this)}>
          {circles}
        </svg>
      
        <div className="button-holder">
          <button onClick={this.handleUndo.bind(this)}
            disabled={undoDisabled}>Undo</button>
          <button onClick={this.handleRedo.bind(this)}
            disabled={redoDisabled}>Redo</button>
        </div>
      
      </div>
    );
  }
  
} 

function renderComponent(props) {
  React.render(<DrawingCanvas {...props} />, document.getElementById('app'));
}

renderComponent();