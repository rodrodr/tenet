HTMLWidgets.widget({

  name: 'plotVoronoiTree',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

          // Create root element
          // https://www.amcharts.com/docs/v5/getting-started/#Root_element
          var root = am5.Root.new(el.id);
          
          
          // Create custom theme...
          var myTheme = am5.Theme.new(root);
          
          // ... no stroke and fill on zero level
          myTheme.rule("Polygon", ["hierarchy", "node", "shape", "depth0"]).setAll({
            strokeOpacity: 0,
            fillOpacity: 0
          });
          
          // ... thick stroke and full opacity on first level
          myTheme.rule("Polygon", ["hierarchy", "node", "shape", "depth1"]).setAll({
            strokeWidth: x.strokeWidthParent,
            fillOpacity: 1,
            stroke: am5.color(x.strokeColor)
          });
          
          // ... no fill and thin stroke on second level
          myTheme.rule("Polygon", ["hierarchy", "node", "shape", "depth2"]).setAll({
            fillOpacity: 0,
            strokeWidth: x.strokeWidth,
            stroke: am5.color(x.strokeColor)
          });
          
          //  ... by default last lever is not clickable, but we change it here, so, add pointer on the last level
          myTheme.rule("HierarchyNode", ["last"]).setAll({
            cursorOverStyle: "pointer"
          });
          
          // ... set global settings for all labels
          myTheme.rule("Label", ["node"]).setAll({
            fontSize: x.font_size,
            minScale: 0.7
          });
          
          // ... hide label of zero level
          myTheme.rule("Label", ["node", "depth0"]).setAll({
            forceHidden: true
          });
          
          // ... hide label of first level
           myTheme.rule("Label", ["node", "depth1"]).setAll({
          //  forceHidden: true
          fontSize: x.font_size_parent,
          forceHidden: x.hide_parent
          });
          
          
          // Set themes
          // https://www.amcharts.com/docs/v5/concepts/themes/
          root.setThemes([
            am5themes_Animated.new(root),
            myTheme
          ]);
          
          
          // Prepare data
          var data = x.data;

// Create series
// https://www.amcharts.com/docs/v5/charts/hierarchy/#Adding
// type tambien rectangle igual de chulo
var series = root.container.children.push(am5hierarchy.VoronoiTreemap.new(root, {
  paddingLeft: 5,
  paddingRight: 5,
  paddingTop: 5,
  paddingBottom: 5,
  singleBranchOnly: true,
  downDepth: x.initialDepth,
  upDepth: 0,
  initialDepth: x.initialDepth,
  valueField: "value",
  categoryField: "name",
  childDataField: "children",
  idField: "name",
  type: x.type,
  cornerCount: x.cornerCount
}));


// WQhen last level node is clicked, zoom to parent
series.nodes.template.events.on("click", function (e) {
  var dataItem = e.target.dataItem ;
  
  
  if (dataItem) {
    if (!dataItem.get("children")) {
      series.selectDataItem(dataItem.get("parent"));
    }
  }
});


// Set data
// https://www.amcharts.com/docs/v5/charts/hierarchy/#Setting_data
series.data.setAll([data]);


// Select root node
// https://www.amcharts.com/docs/v5/charts/hierarchy/#Pre_selected_branch
series.set("selectedDataItem", series.dataItems[0]);


// Make stuff animate on load
series.appear(1000, 100);

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
