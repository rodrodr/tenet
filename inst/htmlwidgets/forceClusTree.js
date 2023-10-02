HTMLWidgets.widget({

  name: 'forceClusTree',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

          var root = am5.Root.new(el.id);

        // Set themes
        // https://www.amcharts.com/docs/v5/concepts/themes/
        root.setThemes([
          am5themes_Animated.new(root)
        ]);


        var data = x.data;

        // Create wrapper container
        var container = root.container.children.push(am5.Container.new(root, {
          width: am5.percent(100),
          height: am5.percent(100),
          layout: root.verticalLayout
        }));
        
        var series = container.children.push(am5hierarchy.ForceDirected.new(root, {
          singleBranchOnly: false,
          downDepth: 2,
          topDepth: 1,
          initialDepth: 4,
          valueField: "value",
          categoryField: "name",
          childDataField: "children",
          idField: "name",
          colorField: "color",
          linkWithField: "linkWith",
          manyBodyStrength: x.BodyStrength,
          minRadius: am5.percent(0),
          maxRadius: am5.percent(x.maxRadius),
          centerStrength: 0.5
        }));
        
         series.circles.template.setAll({
            templateField: "nodeSettings"
        });
        
        series.nodes.template.set("tooltipHTML",x.tooltipText);
        
        series.data.setAll([data]);
    
        series.set("selectedDataItem", series.dataItems[0]);

  
        series.links.template.setAll({
        colorMode: "solid",
        strokeWidth: x.linkWidth,
        strokeOpacity: 1
      });

        // Make stuff animate on load
        series.appear(1000, 100);


      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
