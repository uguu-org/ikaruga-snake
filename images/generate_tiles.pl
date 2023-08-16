#!/usr/bin/perl -w
# Build tiles.png for all snake body tiles by extract unique blocks
# from images.png.  Takes ~5 minutes to run.
#
# Requires imagemagick ("convert" must be available in path).

use strict;
use File::Temp;
use Digest::MD5 qw{md5};

use constant COLORS => (0, 320);
use constant ROTATIONS => (0, 90, 180, 270);
use constant OUTPUT_TILES_PER_ROW => 50;

my @snake_tile_indices0 = qw{
   COLOR_LIGHT
   COLOR_DARK
};
my @snake_tile_indices1 = qw{
   DIRECTION_UP_LEFT
   DIRECTION_UP_UP
   DIRECTION_UP_RIGHT
   DIRECTION_RIGHT_UP
   DIRECTION_RIGHT_RIGHT
   DIRECTION_RIGHT_DOWN
   DIRECTION_DOWN_RIGHT
   DIRECTION_DOWN_DOWN
   DIRECTION_DOWN_LEFT
   DIRECTION_LEFT_DOWN
   DIRECTION_LEFT_LEFT
   DIRECTION_LEFT_UP
};
my @snake_tile_indices2 = qw{
   TYPE_HEAD_FRONT
   TYPE_HEAD_BACK
   TYPE_BODY
   TYPE_TAIL_FRONT
   TYPE_TAIL_BACK
};


if( $#ARGV != 2 )
{
   die "$0 {input.png} {output.png} {output.lua}\n";
}

my ($input_png, $output_png, $output_lua) = @ARGV;

# Table of image MD5 to image index (1-based).
my %image_hash = ();

# Temporary directory for storing intermediate files.
my $temp_dir = File::Temp->newdir();


# Extract a 32x32 block from input, returning index to the output block.
#
# Arguments are (X offset, Y offset, horizontal flip, rotation in degrees)
sub block($$$$)
{
   my ($x, $y, $f, $r) = @_;

   # Extract block as PAM.  We can't use PNG here because imagemagick
   # will embed timestamp information in the output, which makes all the
   # blocks unique.
   my $cmd = "convert $input_png -crop 32x32+$x+$y";
   if( $f != 0 ) { $cmd .= " -flop"; }
   if( $r != 0 ) { $cmd .= " -rotate $r"; }
   open my $pipe, "$cmd pam:-|" or die $!;
   my $pixels = join '', <$pipe>;
   close $pipe;

   # Return index of existing block if we already have one.
   my $md5 = md5($pixels);
   if( exists $image_hash{$md5} )
   {
      print "($x, $y, $f, $r) -> Reused $image_hash{$md5}\n";
      return $image_hash{$md5};
   }

   # Add new block to cache.
   #
   # Note the +1 to make image indices 1-based.
   my $index = (scalar keys %image_hash) + 1;
   $image_hash{$md5} = $index;
   print "($x, $y, $f, $r) -> New -> $index\n";

   # Write block to disk.
   #
   # Alternatively, we can run convert again and tell it to write a PNG.
   # The advantage there would reduced disk IO (~500K vs ~5MB), but
   # the overhead of launching convert twice plus the extra round of
   # PNG decode/encode makes it much slower (~8.5 minutes vs ~5 minutes).
   open my $file, ">$temp_dir/b$index.pam" or die $!;
   print $file $pixels;
   close $file;
   return $index;
}

# Convert an array of indices to lua code.
sub array_to_string(@)
{
   return "{" . (join ", ", @_) . "}";
}

# Generate blank tile.
sub generate_blank()
{
   return "\tblank = " . block(0, 0, 0, 0) . ",\n";
}

# Generate a list of index constants.
sub generate_indices(@)
{
   my (@names) = @_;

   my $indices = "";
   for(my $i = 0; $i < scalar @names; $i++)
   {
      $indices .= "\t$names[$i] = " . ($i + 1) . ",\n";
   }
   return $indices;
}

# Generate a single set of snake tiles for the same body part.
#
# Assumes that 16 steps are available, where next step is +64 to the right
# of the starting position.
sub generate_snake_variations($$$$$)
{
   my ($color, $flip, $rotation, $x, $y) = @_;

   my @blocks = ();
   for(my $step = 0; $step < 16; $step++)
   {
      push @blocks, block($x + $step * 64, $color + $y, $flip, $rotation);
   }

   # Duplicate the final frame.  This avoids index out of bounds when
   # the sub-frame counter increments too fast.
   for(my $dup = 0; $dup < 4; $dup++)
   {
      push @blocks, $blocks[15];
   }

   return "\t\t\t\t" . array_to_string(@blocks) . ",\n";
}

# Generate 16 copies of the snake body tile.
sub generate_snake_replications($$$$$)
{
   my ($color, $flip, $rotation, $x, $y) = @_;

   my @blocks = ();
   my $tile = block($x, $color + $y, $flip, $rotation);
   for(my $step = 0; $step < 16; $step++)
   {
      push @blocks, $tile;
   }

   # Duplicate the final frame.  This avoids index out of bounds when
   # the sub-frame counter increments too fast.
   for(my $dup = 0; $dup < 4; $dup++)
   {
      push @blocks, $blocks[15];
   }

   return "\t\t\t\t" . array_to_string(@blocks) . ",\n";
}

# Generate snake tiles for each available type.
sub generate_snake_for_single_color_direction($$$)
{
   my ($color, $rotation, $rotation_center_index) = @_;

   return
      # Left turn.
      "\t\t\t-- " . $snake_tile_indices1[$rotation_center_index - 1] . "\n" .
      "\t\t\t{\n" .
         # Head forward.
         generate_snake_variations($color, 1, $rotation, 32, 96) .
         # Head back.
         generate_snake_variations($color, 1, $rotation, 0, 96) .
         # Body.
         generate_snake_replications($color, 1, $rotation, 1152, 32) .
         # Tail forward.
         generate_snake_variations($color, 1, $rotation, 0, 224) .
         # Tail back.
         generate_snake_variations($color, 1, $rotation, 0, 256) .
      "\t\t\t},\n" .

      # Forward.
      "\t\t\t-- " . $snake_tile_indices1[$rotation_center_index] . "\n" .
      "\t\t\t{\n" .
         # Head forward.
         generate_snake_variations($color, 0, $rotation, 0, 0) .
         # Head back.
         generate_snake_variations($color, 0, $rotation, 0, 32) .
         # Body.
         generate_snake_replications($color, 0, $rotation, 1088, 32) .
         # Tail forward.
         generate_snake_variations($color, 0, $rotation, 0, 160) .
         # Tail back.
         generate_snake_variations($color, 0, $rotation, 0, 192) .
      "\t\t\t},\n" .

      # Right turn.
      "\t\t\t-- " . $snake_tile_indices1[$rotation_center_index + 1] . "\n" .
      "\t\t\t{\n" .
         # Head forward.
         generate_snake_variations($color, 0, $rotation, 32, 96) .
         # Head back.
         generate_snake_variations($color, 0, $rotation, 0, 96) .
         # Body.
         generate_snake_replications($color, 0, $rotation, 1152, 32) .
         # Tail forward.
         generate_snake_variations($color, 0, $rotation, 0, 224) .
         # Tail back.
         generate_snake_variations($color, 0, $rotation, 0, 256) .
      "\t\t\t},\n";
}

# Generate all snake body tiles, indexed as follows:
# [color][direction][type][step]
sub generate_snake()
{
   my $indices =
      "\t-- snake indices\n" .
      generate_indices(@snake_tile_indices0) .
      generate_indices(@snake_tile_indices1) .
      generate_indices(@snake_tile_indices2);

   $indices .= "\tsnake =\n\t{\n";
   my $ic = 0;
   foreach my $c (COLORS)
   {
      $indices .= "\t\t-- $snake_tile_indices0[$ic]\n\t\t{\n";
      $ic++;
      my $ir = 1;
      foreach my $r (ROTATIONS)
      {
         $indices .= generate_snake_for_single_color_direction($c, $r, $ir);
         $ir += 3;
      }
      $indices .= "\t\t},\n";
   }
   $indices .= "\t},\n";
   return $indices;
}

# Generate tiles for dead expression.
sub generate_dead()
{
   my $indices = "\tdead =\n\t{\n";
   foreach my $c (COLORS)
   {
      my @blocks = ();
      foreach my $r (ROTATIONS)
      {
         push @blocks, block(1024, $c + 32, 0, $r);
      }
      $indices .= "\t\t" . array_to_string(@blocks) . ",\n";
   }
   $indices .= "\t},\n";
   return $indices;
}

# Generate tiles for fruits.  No rotations for these.
sub generate_fruits()
{
   my $indices = "\tgrow_friendly_fruits =\n\t{\n";
   foreach my $c (COLORS)
   {
      my @tiles = (
         block(1472, $c + 96, 0, 0),
         block(1408, $c + 96, 0, 0),
         block(1344, $c + 96, 0, 0),
         block(1024, $c + 96, 0, 0)
      );
      $indices .= "\t\t" . array_to_string(@tiles) . ",\n";
   }
   $indices .= "\t},\n";

   $indices .= "\tgrow_hostile_fruits =\n\t{\n";
   foreach my $c (COLORS)
   {
      my @tiles = (
         block(1472, $c + 160, 0, 0),
         block(1408, $c + 160, 0, 0),
         block(1344, $c + 160, 0, 0),
         block(1216, $c + 160, 0, 0)
      );
      $indices .= "\t\t" . array_to_string(@tiles) . ",\n";
   }
   $indices .= "\t},\n";

   $indices .= "\teat_fruits =\n\t{\n";
   foreach my $c (COLORS)
   {
      my @tiles = (
         block(1088, $c + 96, 0, 0),
         block(1152, $c + 96, 0, 0),
         block(1216, $c + 96, 0, 0),
         block(1280, $c + 96, 0, 0)
      );
      $indices .= "\t\t" . array_to_string(@tiles) . ",\n";
   }
   $indices .= "\t},\n";

   $indices .= "\tchange_fruits =\n\t{\n";
   foreach my $c (COLORS)
   {
      my @tiles = (
         block(1024, $c + 160, 0, 0),
         block(1088, $c + 160, 0, 0),
         block(1152, $c + 160, 0, 0),
         block(1216, $c + 160, 0, 0)
      );
      $indices .= "\t\t" . array_to_string(@tiles) . ",\n";
   }
   $indices .= "\t},\n";
   return $indices;
}

# Collect all unique tiles into one image.
sub build_output_image($)
{
   my ($output_png) = @_;

   my $image_count = scalar keys %image_hash;
   my $blank = block(0, 0, 0, 0);

   my @rows = ();
   my $row_index = 0;
   for(my $i = 0; $i < $image_count; $i += OUTPUT_TILES_PER_ROW)
   {
      my @cells = ();
      for(my $j = 0; $j < OUTPUT_TILES_PER_ROW; $j++)
      {
         if( $i + $j < $image_count )
         {
            push @cells, (sprintf '%s/b%d.pam', $temp_dir, $i + $j + 1);
         }
         else
         {
            push @cells, "$temp_dir/b$blank.pam";
         }
      }

      # Build a single row.
      my $row_image = "$temp_dir/r$row_index.png";
      my $cmd = "convert +append " . (join " ", @cells) . " $row_image";
      system $cmd;
      push @rows, $row_image;
      $row_index++;
   }

   # Combine all rows.
   my $cmd = "convert -append " . (join " ", @rows) . " $output_png";
   system $cmd;
}


my $indices = "tiles =\n{\n" .
              generate_blank() .
              generate_snake() .
              generate_dead() .
              generate_fruits() .
              "}\n";

open my $outfile, ">$output_lua" or die $!;
print $outfile $indices;
close $outfile;

build_output_image($output_png);
